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
  t = np.array([[ cos,  sin, 0.,   0.,   0., 0.],
                [-sin,  cos, 0.,   0.,   0., 0.],
                [0.,     0., 1.,   0.,   0., 0.],
                [0.,     0., 0.,  cos,  sin, 0.],
                [0.,     0., 0., -sin,  cos, 0.],
                [0.,     0., 0.,   0.,   0., 1.]], dtype = float)

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
