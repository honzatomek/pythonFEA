#!/usr/bin/python3

import numpy as np
np.set_printoptions(precision=4, suppress=True)

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
rho = 1.

# preparing the global vectors and matrices
f = np.zeros((ndof, 1), dtype=float)
k = np.zeros((ndof, ndof), dtype=float)
r = np.zeros((ndof, 1), dtype=float)
u = np.zeros((ndof, 1), dtype=float)

# elemental stiffness matrix
ke_loc = []
tmat = []
for i in range(nelem):
  # length
  l = ((xz[el[i, 1], 0] - xz[el[i, 0], 0]) ** 2. + (xz[el[i, 1], 1] - xz[el[i, 0], 1]) ** 2.) ** (1./2.)
  l2=l*l;
  l3=l2*l;
  # element stiffness matrix in lcs
  ke = np.array([[ EA/l,       0.,      0., -EA/l,     0.,    0.],
           [   0.,  12.*EI/l3, -6.*EI/l2,  0., -12.*EI/l3, -6.*EI/l2],
           [   0.,  -6.*EI/l2,   4.*EI/l,  0.,   6.*EI/l2,   2.*EI/l],
           [-EA/l,       0.,      0.,  EA/l,     0.,    0.],
           [   0., -12.*EI/l3,  6.*EI/l2,  0.,  12.*EI/l3,  6.*EI/l2],
           [   0.,  -6.*EI/l2,   2.*EI/l,  0.,   6.*EI/l2,  4.*EI/l]], dtype=float)

  ke_loc.append(ke)

  # cosine and sine
  c = (xz[el[i, 1], 0] - xz[el[i, 0], 0]) / l
  s = (xz[el[i, 1], 1] - xz[el[i, 0], 1]) / l

  # transformation matrix
  t = np.array([[ c,  s, 0., 0., 0., 0.],
          [-s,  c, 0., 0., 0., 0.],
          [0., 0., 1., 0., 0., 0.],
          [0., 0., 0.,  c,  s, 0.],
          [0., 0., 0., -s,  c, 0.],
          [0., 0., 0., 0., 0., 1.]], dtype = float)

  tmat.append(t)

  # element stiffness matrix in GCS
  ke = t.T @ ke @ t

  # populate the stiffness matrix
  k[np.ix_(lme[i], lme[i])] += ke

ke_loc = np.array(ke_loc, dtype=float)
tmat = np.array(tmat, dtype=float)

# assemble nodal loads
for i in range(fn.shape[0]):
  f[int(fn[i, 0])] = fn[i, 1]

# assemble prescribed displacements
for i in range(nprescribed):
  u[int(prescribed[i, 0])] = prescribed[i, 1]

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
ur = u[:ndet]
uu = u[ndet:]
fr = f[:ndet]
fu = f[ndet:]

# check for rigid body modes
eval, _ = np.linalg.eigh(kuu)
eval = np.sort(eval)
print(f'eig(K) = {eval}')

# solve for displacements
uu = np.linalg.inv(kuu) @ (fu - kru.T @ ur)

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

