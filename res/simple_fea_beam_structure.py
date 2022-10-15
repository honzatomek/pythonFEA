#!/usr/bin/python3

import numpy as np
np.set_printoptions(precision=4, suppress=True)


class BaseTemplateID:
  def __init__(self, id: int, label: str=None):
    self.__id = int(id)
    if label is not None:
      self.__label = str(label)
    else:
      self.__label = None

  @property
  def id(self) -> int:
    return self.__id

  @property
  def label(self) -> str:
    return self.__label

class BaseTemplateName:
  def __init__(self, name: str, label: str=None):
    self.__name = str(name)
    if label is not None:
      self.__label = str(label)
    else:
      self.__label = None

  @property
  def name(self) -> str:
    return self.__name

  @property
  def label(self) -> str:
    return self.__label


class BaseTemplateSet:
  def __init__(self, ObjType, objects: list=None):
    self.__Type = ObjType
    self.__objects = {}
    self.__count = 0
    if objects is not None:
      self.add(objects)

  @property
  def count(self):
    return self.__count

  @property
  def ids(self):
    return self.__objects.keys()

  def __getitem__(self, id: int):
    return self.__objects[id]

  def __add__(self, object: list):
    if object[0] not in self.ids:
      self.__objects[object[0]] = self.__Type(*object)
      self.__count += 1
    else:
      raise ValueError(f'Duplicite {self.__Type.__name__:s} ID {str(o[0]):s}.')

  def add(self, objects: list):
    for o in objects:
      self.__add__(o)


class Set(BaseTemplateName):
  def __init__(self, name: str, ids: list=None, label: str=None):
    super().__init__(name, label=
    self.__ids = set()
    if ids is not None:
      self.add(ids)

  @property
  def ids(self) -> list:
    return list(self.__ids)

  def __add__(self, id):
    if type(id) is int:
      ids = list([id])
    else:
      ids = list(id)
    self.__ids = set().union(self.__ids, ids)

  def add(self, ids):
    self.__add__(ids)


class Node2D(BaseTemplateID):
  def __init__(self, id, x, z, label=None):
    super().__init__(id, label)
    self.__coors = [float(x), float(z)]
    self.__dofs = [-1, -1, -1]

  def __getitem__(self, dof: int) -> int:
    return self.__dofs[dof-1]

  def __setitem__(self, dof: int, dofid: int):
    self.__dofs[dof-1] = int(dofid)

  @property
  def coors(self) -> int:
    return self.__coors

  @property
  def x(self) -> float:
    return self.__coors[0]

  @property
  def z(self) -> float:
    return self.__coors[1]

  @property
  def dofs(self) -> list:
    return self.__dofs

  @dofs.setter
  def dofs(self, dofs: list):
    self.__dofs = list(dofs)


class Nodes2D(BaseTemplateSet):
  def __init__(self, nodes: list=None):
    super().__init__(Node2D, nodes)

  def distance(self, id1, id2) -> float:
    return ((self.__nodes[id2].x - self.__nodes[id1].x) ** 2. +
            (self.__nodes[id2].z - self.__nodes[id1].z) ** 2.) ** 0.5


class Constraint2D(BaseTemplateID):
  def __init__(self, id: int, dofs: list, label: str=None):
    super().__init(id, label)
    self.__dofs = [False, False, False]
    for i in range(len(dofs)):
      self.__dofs[dofs[i]-1] = True

  @property
  def dofs(self) -> list:
    return [i + 1 for i in range(len(self.__dofs)) if self.__dofs[i]]

  def __getitem__(self, dof):
    return self.__dofs[dof-1]


class Prescribed2D(BaseTemplateID):
  def __init__(self, id: int, dofs: list, values: list,  label: str=None):
    super().__init(id, label)
    self.__dofs = {}
    for i in range(len(dofs)):
      self.__dofs[dofs[i]-1] = values[i]

  @property
  def dofs(self) -> dict:
    return self.__dofs

  def __getitem__(self, dof) -> float:
    return self.__dofs[dof-1]


class Constraints2D(BaseTemplateSet):
  def __init__(self, constraints: list=None):
    super().__init__(Constraint2D, constraints)


class PrescribedDOFs2D(BaseTemplateSet):
  def __init__(self, prescribed: list=None):
    super().__init__(Prescribed2D, prescribed)


class Constraints(BaseTemplateName):
  def __init__(self, name: str, constraints: list=None, prescribed: list=None, label: str=None):
    super().__init__(name, label)
    self.__constraints = Constraints2D()
    self.__prescribed = PrescribedDOFs2D()
    if constraints is not None:
      self.__constraints.add(constraints)
    if prescibed is not None:
      self.__prescribed.add(prescribed)

  def add_constraints(self, constraints: list):
    self.__constraints.add(constraints)

  def add_prescribed(self, prescribed):
    self.__prescribed.add(prescribed)


class MaterialISO(BaseTemplateName):
  def __init__(self, name: str, E: float, nu: float, rho: float, label: str= None):
    super().__init__(name, label)
    self.__E = float(E)
    self.__nu = float(nu)
    self.__rho = float(rho)

  @property
  def E(self) -> float:
    return self.__E

  @property
  def nu(self) -> float:
    return self.__nu

  @property
  def rho(self) -> float:
    return self.__rho


class Materials(BaseTemplateSet):
  def __init__(self, materials=None):
    super().__init__(MaterialISO, materials)


class PBeam2D(BaseTemplateName):
  def __init__(self, name: str, A: float, I: float, label: str=None):
    super().__init__(name, label)
    self.__A = float(A)
    self.__I = float(I)

  @property
  def A(self) -> float:
    return self.__A

  @property
  def I(self) -> float:
    return self.__I


class Properties(BaseTemplateSet):
  def __init__(self, properties=None):
    super().__init__(PBeam2D, properties)


class System(BaseTemplateName):
  def __init__(self, name: str, assign: list=None, label: str=None):
    super().__init__(name, label)
    self.__damping = None
    self.__assign = {}

  def __add__(self, assignment):
    eID, pID, mID = assignment
    self.__assign[eID] = [int(pID), int(mID)]

  def __getitem__(self, id) -> list:
    return self.__assign[id]

  @property
  def damping(self):
    return self.__damping

  @damping.setter
  def damping(self, rayleigh: list):
    alpha = rayleigh[0]
    beta = rayleigh[1]
    self.__damping = [float(alpha), float(beta)]

  @property
  def assign(self):
    return self.__assign

  def add(self, assignment: list):
    for a in assignment:
      self.__add__(a)


class Beam2D(BaseTemplateID):
  def __init__(self, id: int, nd1: int, nd2: int, label: str=None):
    super().__init__(id, label)
    self.__nodes = [int(nd1), int(nd2)]

  @property
  def nodes(self):
    return self.__nodes


class Structure:
  def __init__(self, name):
    self.name = name


# damping coefficients
alpha = 0.02
beta = 0.02

structure = Structure('beam2D', [alpha, beta])

# property
A = 1000.
I = 1000000.

# material
E = 210000.
nu = 0.3
rho = 7.85e-9

# property and material
structure.add_property(1, A, I)
structure.add_material(1, E, nu, rho)

# nodes
structure.add_node(1, 0., 0.)
structure.add_node(2, 3000., 0.)
structure.add_node(3, 6000., 0.)

# elements
structure.add_element(1, 1, 1, [0, 0, 0], [0, 0, 0], 1, 2)
structure.add_element(2, 1, 1, [0, 0, 0], [0, 0, 0], 2, 3)

# constraints
structure.add_constraint(1, [1, 2, 3])

# localisation matrix
structure.consolidate()
print(structure.lme)

exit()

# nodal loads (dof, load)
fn = np.array([[7, 1.]], dtype=float)

# number of constrained dofs (from beginning)
nconstraints = 3

# number of prescribed dofs (after constraints) [dof, displacement]
nprescribed = 0
prescibed = np.zeros((1, 2), dtype = float)

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
