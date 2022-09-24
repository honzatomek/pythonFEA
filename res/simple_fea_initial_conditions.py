#!/usr/bin/python3

import numpy as np
np.set_printoptions(precision=4, suppress=True)
from scipy.linalg import eigh
from scipy.integrate import odeint

import math


def dampimg_ratio(omega, k, m):  # zeta
  return omega / (2. * (k * m) ** (0.5))

def phase_ext_force(zeta, omega, omegan):
  return math.atan((-2 * zeta * omega / omegan) / (1. - (omega ** 2.) / (omegan ** 2.)))

def get_rayleigh_damping(zeta1, zeta2, omega1, omega2):
  C = np.array([[omega1, 1. / omega1],
                [omega2, 1. / omega2]], dtype=float) / 2.
  Z = np.array([[zeta1], [zeta2]], dtype=float)
  D = (np.linalg.inv(C) @ Z).flatten()
  alpha = D[0]
  beta = D[1]
  return alpha, beta

def rayleigh_damping_ratio(omega, alpha, beta):
  return 0.5 * (alpha * omega + beta / omega)

def C_rayleigh(omegas, alpha, beta):
  o = omegas.flatten()
  C = np.zeros((o.shape[0], o.shape[0]), dtype=float)
  for i in range(o.shape[0]):
    C[i,i] = rayleigh_damping_ratio(o[i], alpha, beta)
  return C

def damping_ratio_from_ln_decay(x0, x1):
  d = math.ln(x0 / x1)
  zeta = d / (d ** 2. + (4. * math.pi) ** 2.) ** 0.5
  return zeta


# nodes
xz = np.array([[0., 0.],
               [3., 0.],
               [6., 0.]], dtype=float)

# elements
el = np.array([[0, 1],
               [1, 2]], dtype=int)

# localisation matrix
lme = np.array([[0, 1, 2, 3, 4, 5],
                [3, 4, 5, 6, 7, 8]], dtype=int)

# nodal loads (dof, load)
fn = np.array([[7, 1.]], dtype=float)

# number of elements
nelem = el.shape[0]

# number of degrees of freedom
ndof =  len(set(lme.flatten()))

# number of constrained dofs (from beginning)
nconstraints = 3

# number of prescribed dofs (after constraints) [dof, displacement]
nprescribed = 0
prescibed = np.zeros((1, 2), dtype = float)

# material properties
EA = 1.
EI = 1.
rho = 1.  # mass per unit length

# damping coefficients
alpha = 0.02
beta = 0.02

# max number of modes for modal decomposition
nmodes = 10

# preparing the global vectors and matrices
f = np.zeros((ndof, 1), dtype=float)
k = np.zeros((ndof, ndof), dtype=float)
m = np.zeros((ndof, ndof), dtype=float)
c = np.zeros((ndof, ndof), dtype=float)
r = np.zeros((ndof, 1), dtype=float)
u = np.zeros((ndof, 1), dtype=float)

# elemental stiffness matrix
ke_loc = []
tmat = []
for i in range(nelem):
  # length
  l = ((xz[el[i, 1], 0] - xz[el[i, 0], 0]) ** 2. + (xz[el[i, 1], 1] - xz[el[i, 0], 1]) ** 2.) ** (1./2.)
  l2 = l * l
  l3 = l2 * l
  # element stiffness matrix in lcs
  ke = np.array([[ EA/l,       0.,      0., -EA/l,     0.,    0.],
                 [   0.,  12.*EI/l3, -6.*EI/l2,  0., -12.*EI/l3, -6.*EI/l2],
                 [   0.,  -6.*EI/l2,   4.*EI/l,  0.,   6.*EI/l2,   2.*EI/l],
                 [-EA/l,       0.,      0.,  EA/l,     0.,    0.],
                 [   0., -12.*EI/l3,  6.*EI/l2,  0.,  12.*EI/l3,  6.*EI/l2],
                 [   0.,  -6.*EI/l2,   2.*EI/l,  0.,   6.*EI/l2,  4.*EI/l]], dtype=float)

  ke_loc.append(ke)

  # element mass matrix in lcs
  me = rho * l / 420. * np.array([[140.,     0.,     0.,  70.,     0.,     0.],
                                  [  0.,   156.,  22.*l,   0.,    54., -13.*l],
                                  [  0.,  22.*l,  4.*l2,   0.,  13.*l, -3.*l2],
                                  [ 70.,     0.,     0., 140.,     0.,     0.],
                                  [  0.,    54.,  13.*l,   0.,   156., -22.*l],
                                  [  0., -13.*l, -3.*l2,   0., -22.*l,  4.*l2]], dtype=float)

  # element damping matrix
  ce = alpha * me + beta * ke

  # cosine and sine
  cos = (xz[el[i, 1], 0] - xz[el[i, 0], 0]) / l
  sin = (xz[el[i, 1], 1] - xz[el[i, 0], 1]) / l

  # transformation matrix
  t = np.array([[ cos,  sin, 0., 0., 0., 0.],
          [-sin,  cos, 0., 0., 0., 0.],
          [0., 0., 1., 0., 0., 0.],
          [0., 0., 0.,  cos,  sin, 0.],
          [0., 0., 0., -sin,  cos, 0.],
          [0., 0., 0., 0., 0., 1.]], dtype = float)

  tmat.append(t)

  # element stiffness matrix in GCS
  ke = t.T @ ke @ t
  me = t.T @ me @ t
  ce = t.T @ ce @ t

  # populate the stiffness matrix
  k[np.ix_(lme[i], lme[i])] += ke
  m[np.ix_(lme[i], lme[i])] += me
  c[np.ix_(lme[i], lme[i])] += ce

ke_loc = np.array(ke_loc, dtype=float)
tmat = np.array(tmat, dtype=float)

# assemble nodal loads
for i in range(fn.shape[0]):
  f[int(fn[i, 0])] = fn[i, 1]

# divide the stiffness matrix and vectors
# [ Krr,  Kru ] x [ ur ] = [ fr ]
# [ Kur,  Kuu ]   [ uu ]   [ fu ]
#
#  Krr @ ur + Kru @ uu = fr
#  Kur @ ur + Kuu @ uu = fu
#  uu = Kuu-1 @ ( fu - Kur @ ur )
#  fr = Krr @ ur + Kru @ uu

ndet = nconstraints + nprescribed
krr = k[:ndet, :ndet]
kru = k[:ndet, ndet:]
# kur = k[ndet:, :ndet] kur = kru.T
kuu = k[ndet:, ndet:]
muu = m[ndet:, ndet:]
ur = u[:ndet]
uu = u[ndet:]
fr = f[:ndet]
fu = f[ndet:]

# check for rigid body modes
omega, evec = eigh(kuu, eigvals_only=False, subset_by_value=(-np.inf, 1.0e-3))
if len(omega) > 0:
  print(f'[E] Rigid body modes found: {len(omega):n}')
  for i in range(len(omega)):
    print(f'  Eigenfrequency: {omega[i] / (2. * math.pi):.3f} Hz')
    u[ndet:] = evec[:,i].reshape(uu.shape)
    print(f'  {"dof":^8s} {"disp":^12s}')
    for j in range(len(u)):
      print(f'  {j:8n} {u[j,0]:12.3e}')
  exit()

# solve for displacements = initial conditions
uu = np.linalg.inv(kuu) @ (fu - kru.T @ ur)
u[ndet:] = uu

#
ndet = nconstraints
krr = k[:ndet, :ndet]
kru = k[:ndet, ndet:]
# kur = k[ndet:, :ndet] kur = kru.T
kuu = k[ndet:, ndet:]
muu = m[ndet:, ndet:]
cuu = m[ndet:, ndet:]
ur = u[:ndet]
uu = u[ndet:]
fr = f[:ndet]
fu = f[ndet:]

# transform to modal domain
omega, evec = eigh(kuu, muu, eigvals_only=False, subset_by_index=(0, min(ndof - ndet, nmodes) - 1))
# km = evec.T @ kuu @ evec
# mm = evec.T @ muu @ evec
# cm = evec.T @ cuu @ evec
km = np.diag(evec.T @ kuu @ evec)
mm = np.diag(evec.T @ muu @ evec)
cm = np.diag(evec.T @ cuu @ evec)
if evec.shape[0] == evec.shape[1]:
  eveci = np.linalg.inv(evec)  # inverse
else:
  eveci = np.linalg.pinv(evec) # pseudoinverse
q_0 = eveci @ uu   # uu = evec @ q0
q_1 = np.zeros(q_0.shape)  # no initial velocities
q_2 = np.zeros(q_0.shape)  # no initial acceleration
# print(km)
# print(mm)
# print(cm)
# print(q_0)
# print(q_0)

N = 1
t0 = 0.
tn = 10.
h = (tn - t0) / N
# print(h)
q0 = np.zeros((q_0.shape[0], N + 1), dtype=float)
q1 = np.zeros((q_1.shape[0], N + 1), dtype=float)
q2 = np.zeros((q_2.shape[0], N + 1), dtype=float)

q0[:,0] = q_0[:,0]
q1[:,0] = q_1[:,0]
q2[:,0] = q_2[:,0]

# print(q0)


A = -(cm/mm).reshape((q0.shape[0], 1))
B = -(km/mm).reshape((q0.shape[0], 1))
print(A)
print(B)
for i in range(1):
  u0 = q0[:,i].reshape((q0.shape[0], 1))
  v0 = q1[:,i].reshape((q0.shape[0], 1))
  a0 = A * v0 + B * u0

  print(f'u0 =\n{u0}')
  print(f'v0 =\n{v0}')
  print(f'a0 =\n{a0}')

  u1 = u0 + v0 * h / 2
  v1 = v0 + a0 * h / 2
  a1 = A * v1 + B * u1

  print(f'u1 =\n{u1}')
  print(f'v1 =\n{v1}')
  print(f'a1 =\n{a1}')

  u2 = u0 + v1 * h / 2
  v2 = v0 + a1 * h / 2
  a2 = A * v1 + B * u1

  print(f'u2 =\n{u2}')
  print(f'v2 =\n{v2}')
  print(f'a2 =\n{a2}')

  u3 = u0 + v2 * h
  v3 = v0 + a2 * h
  a3 = A * v2 + B * u2

  print(f'u3 =\n{u3}')
  print(f'v3 =\n{v3}')
  print(f'a3 =\n{a3}')

  print((u0 + h/6 * (v0 + 2 * (v1 + v2) + v3)))

  q0[:,i+1] = (u0 + h/6 * (v0 + 2 * (v1 + v2) + v3))[:,0]
  q1[:,i+1] = (v0 + h/6 * (a0 + 2 * (a1 + a2) + a3))[:,0]
  q2[:,i+1] = (A * q1[:,i+1].reshape((q0.shape[0], 1)) + B * q0[:,i+1].reshape((q0.shape[0], 1)))[:,0]

  print(q0[:,:i+1+1])
  print(q1[:,:i+1+1])
  print(q2[:,:i+1+1])

  # k0 = h * q1[:,i]
  # l0 = h * (A * q1[:,i] + B * q0[:,i])
  # k1 = h * (q1[:,0] + k0 / 2)
  # l1 = h * (A * (q1[:,i] + k0 / 2) + B * (q0[:,i] + l0/2))
  # print(k1)
  # print(l1)

exit()

# equation:
# km * q(t) + cm * q'(t) + mm * q"(t) = 0
# q"(t) = -mm-1 * cm * q'(t) -mm-1 * km * q(t)
# q"(t) = A * q'(t) + B * q(t)
# A = -mm-1 * cm
# B = -mm-1 * km
# q'(t) = z(t)
# q"(t) = A * z(t) + B * q(t)

def solver(y, t):
  # y = [r, s]
  # return r', s'
  return [y[1], A * y[0] - B * y[1]]

t = np.arange(0., 10., 0.5)
sol = []
for i in range(q0.shape[0]):
  A = -cm[i, i] / mm[i, i]
  B = -km[i, i] / mm[i, i]
  solver = lambda y, t: [y[1], A * y[0] - B * y[1]]
  print(f'i = {i+1:n}')
  print(odeint(solver, [q0[i, 0], q_0[i, 0]], t))
  sol.append(odeint(solver, [q0[i, 0], q_0[i, 0]], t))
# print(sol)

exit()

# solve for reactions
fr = krr @ ur + kru @ uu

# reconstruction of whole vectors
u[ndet:] = uu
f[:ndet] = fr

print(f'global displacements u =\n{u.T[0]}')
print(f'reactions fr =\n{fr.T[0]}')

# internal forces
inf = []
for i in range(nelem):
  # elemental displacements
  r = u[lme[i]]
  # inner forces
  s = ke_loc[i] @ tmat[i] @ r
  print(f'element {i+1:n} inner forces =\n{s.T[0]}')
  inf.append(s.T[0])
inf = np.array(inf, dtype=float)

