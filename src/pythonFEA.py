import math
import numpy as np
import logging

BASIC_FORMAT = logging.Formatter('%(message)s')
ADVANCED_FORMAT = logging.Formatter('%(asctime)6s *%(levelname).1s* %(message)s', datefmt='%H%M%S')

logging.basicConfig(level=logging.INFO, format='%(asctime)6s *%(levelname).1s* %(message)s', datefmt='%H%M%S')

class BaseError(Exception):
  def __init__(self, message):
    self.message = message
    logging.exception(message)
    super().__init__(self.message)

class MissingIndex(BaseError):
  pass

class UsedIndex(BaseError):
  pass

class WrongType(BaseError):
  pass

class NotInitialised(BaseError):
  pass

class Node:
  def __init__(self, id, x, y, z, label = None):
    self.__id = id
    self.__x = x
    self.__y = y
    self.__z = z
    self.__label = label

  @property
  def id(self):
    return self.__id

  @id.setter
  def id(self, id):
    self.__id = id

  @property
  def x(self):
    return self.__x

  @x.setter
  def x(self, x):
    self.__x = x

  @property
  def y(self):
    return self.__y

  @y.setter
  def y(self, y):
    self.__y = y

  @property
  def z(self):
    return self.__z

  @z.setter
  def z(self, z):
    self.__z = z

  @property
  def label(self):
    return self.__label

  @label.setter
  def label(self, label):
    self.__label = label

  @property
  def xyz(self):
    return np.asarray([self.x, self.y, self.z], dtype=float)


class Nodes:
  def __add(self, node):
    if node is None:
      pass
    if type(node) is not Node:
      raise WrongType(f'Node is not of type Node, but {type(node)} - {str(node)}.')
    if node.id not in self.__items:
      raise UsedIndex(f'Node ID {node.id} already exists.')
    self.__items.setdefault(node.id, node)
    self.__count += 1

  def __init__(self):
    self.__count = 0
    self.__items = dict()

  def id(self, id):
    if id in self.__items:
      return self.__items[id]
    else:
      return False

  def add(self, id = None, x = None, y = None, z = None, label = None, node = None):
    if id is None or x is None or y is None or z is None:
      self.__add(node)
    else:
      self.__add(Node(id, x, y, z, label))

  @property
  def count(self):
    return self.__count

  def distance(self, id1, id2):
    n1 = self.id(id1)
    n2 = self.id(id2)
    return math.sqrt((n2.x - n1.x) ^ 2 + (n2.y - n1.y) ^ 2 + (n2.z - n1.z) ^ 2)


class Beam2D:
  def __init__(self, id, pid, mid, nid1, nid2, rx1 = False, rz1 = False, rfi1 = False, rx2 = False, rz2 = False, rfi2 = False, label = None, mi = 0.5):
    self.__id = id
    self.__n1 = n1
    self.__n2 = n2
    self.__r1 = [rx1, rz1, rfi1]
    self.__r2 = [rx2, rz2, rfi2]
    self.__p = pid
    self.__m = mid
    self.__label = label

    self.__l = None
    self.__loc = None

    # ratio between lumped and consistent mass matrix
    self.__mi = 0.5

    self.__nodes_ref = False
    self.__prop_ref = False
    self.__mat_ref = False

  @property
  def id(self):
    return self.__id

  @id.setter
  def id(self, id):
    self.__id = id

  @property
  def n1(self):
    return self.__n1

  @n1.setter
  def n1(self, n1):
    self.__n1 = n1

  @property
  def n2(self):
    return self.__n2

  @n2.setter
  def n2(self, n2):
    self.__n2 = n2

  @property
  def r1(self):
    return self.__r1

  @property
  def r2(self):
    return self.__r2

  @property
  def pid(self):
    return self.__pid

  @pid.setter
  def pid(self, pid):
    self.__pid = pid

  @property
  def mid(self):
    return self.__mid

  @mid.setter
  def mid(self, mid):
    self.__mid = mid

  @property
  def label(self):
    return self.__label

  @label.setter
  def label(self, label):
    self.__label = label

  @property
  def mi(self):
    return self.__mi

  @mi.setter
  def mi(self, mi):
    self.__mi = mi

  def init_nodes(self, nodes):
    n = nodes.id(self.__n1)
    if n:
      self.__n1 = n
    else:
      raise MissingIndex(f'Node ID {self.__n1} is missing.')

    n = nodes.id(self.__n2)
    if n:
      self.__n2 = n
    else:
      raise MissingIndex(f'Node ID {self.__n2} is missing.')

    self.__l = nodes.distance(self.__n1.id, self.__n2.id)
    self.__nodes_ref = True

  def init_prop(self, props):
    p = props.id(self.__p)
    if p:
      self.__p = p
    else:
      raise MissingIndex(f'Property ID {self.__p} is missing.')

    self.__prop_ref = True

  def init_mat(self, mats):
    m = mats.id(self.__m)
    if m:
      self.__m = m
    else:
      raise MissingIndex(f'Material ID {self.__p} is missing.')

    self.__mat_ref = True

  def init(self, nodes, props, mats):
    self.init_nodes(nodes)
    self.init_prop(props)
    self.init_mat(mats)

  @property
  def n1(self):
    if not self.__nodes_ref:
      raise NotInitialised(f'Element ID {self.id} Nodes have not been initialised.')
    return self.__nd1

  @property
  def n2(self):
    if not self.__nodes_ref:
      raise NotInitialised(f'Element ID {self.id} Nodes have not been initialised.')
    return self.__nd2

  @property
  def l(self):
    if not self.__nodes_ref:
      raise NotInitialised(f'Element ID {self.id} Nodes have not been initialised.')
    return self.__l

  @property
  def A(self):
    if not self.__prop_ref:
      raise NotInitialised(f'Element ID {self.id} Property has not been initialised.')
    return self.__prop.A

  @property
  def I(self):
    if not self.__prop_ref:
      raise NotInitialised(f'Element ID {self.id} Property has not been initialised.')
    return self.__prop.I

  @property
  def nsm(self):
    if not self.__prop_ref:
      raise NotInitialised(f'Element ID {self.id} Property has not been initialised.')
    return self.__prop.nsm

  @property
  def E(self):
    if not self.__mat_ref:
      raise NotInitialised(f'Element ID {self.id} Material has not been initialised.')
    return self.__mat.E

  @property
  def ro(self):
    if not self.__mat_ref:
      raise NotInitialised(f'Element ID {self.id} Material has not been initialised.')
    return self.__mat.ro

  @property
  def alpha(self):
    if not self.__mat_ref:
      raise NotInitialised(f'Element ID {self.id} Material has not been initialised.')
    return self.__mat.alpha

  @property
  def initialised(self):
    if self.__nodes_ref and self.__prop_ref and self.__mat_ref:
      return True
    else:
      return False

  @property
  def t(self):
    c = (self.n2.x - self.n1.x) / self.l
    s = (self.n2.y - self.n1.y) / self.l

    t = np.array([[c, s, 0.0, 0.0, 0.0, 0.0],
                  [-s, c, 0.0, 0.0, 0.0, 0.0],
                  [0.0, 0.0, 1.0, 0.0, 0.0, 0.0],
                  [0.0, 0.0, 0.0, c, s, 0.0],
                  [0.0, 0.0, 0.0, -s, c, 0.0],
                  [0.0, 0.0, 0.0, 0.0, 0.0, 1.0]],
                 dtype=float)

    return t

  @property
  def k_lcs(self):
    l = self.l
    A = self.A
    I = self.I
    E = self.E

    EA = E * A
    EI = E * I
    l2 = l * l
    l3 = l2 * l

    ke = np.array([[EA / l, 0.0, 0.0, -EA / l, 0.0, 0.0],
                   [0.0, 12.0 * EI / l3, -6.0 * EI / l2, 0.0, -12.0 * EI / l3, -6.0 * EI / l2],
                   [0.0, -6.0 * EI / l2, 4.0 * EI / l, 0.0, 6.0 * EI / l2, 2.0 * EI / l],
                   [-EA / l, 0.0, 0.0, EA / l, 0.0, 0.0],
                   [0.0, -12.0 * EI / l3, 6.0 * EI / l2, 0.0, 12.0 * EI / l3, 6.0 * EI / l2],
                   [0.0, -6.0 * EI / l2, 2.0 * EI / l, 0.0, 6.0 * EI / l2, 4.0 * EI / l]],
                  dtype=float)

    return ke

  @property
  def ke(self):
    # local element stiffness
    kl = self.k_lcs
    # transformation matrix
    t = self.t
    # transformation LCS -> GCS
    ke = t.T @ kl @ t

    return ke

  @property
  def m_lumped(self):
    # structural mass
    sm = self.A * self.l * self.ro
    # nonstructural mass
    nm = self.l * self.nsm
    # local lumped element mass matrix
    mle = np.eye(6, dtype=float) * (sm + nm) / 2
    mle[2][2] = 0.0
    mle[5][5] = 0.0

    return mle

  @property
  def m_consistent(self):
    l = self.l
    l2 = l ** l
    # structural mass
    sm = self.A * l * self.ro
    # nonstructural mass
    nm = l * self.nsm
    # local lumped element mass matrix
    mce = np.zeros((6, 6), dtype=float)
    mce = np.array([[140.0, 0.0, 0.0, 70.0, 0.0, 0.0],
                    [0.0, 156.0, 22.0 * l, 0.0, 54.0, -13.0 * l],
                    [0.0, 22.0 * l, 4.0 * l2, 0.0, 13.0 * l, -3.0 * l2],
                    [70.0, 0.0, 0.0, 140.0, 0.0, 0.0],
                    [0.0, 54.0, 13.0 * l, 0.0, 156.0, -22.0 * l],
                    [0.0, -13.0 * l, -3.0 * l2, 0.0, -22.0 * l, 4.0 * l2]], dtype=float)
    mce *= (sm + nm) / 420.0

    return mce

  @property
  def m(self):
    # get mass in LCS
    ml = (1.0 - self.mi) * self.m_consistent + self.mi * self.m_lumped
    # transformation matrix
    t = self.t

    # transformation LCS -> GCS
    me = t.T @ ml @ t

    return me

  def lf_lcs(self, fx = 0.0, fz = 0.0):
    l = self.l
    l2 = l * l
    # load vector in LCS
    fl = np.array([fx * l / 2.0,
                   -fz * l / 2.0,
                   1 / 12 * fz * l2,
                   fx * l,
                   - fz * l / 2.0,
                   - 1 / 12 * fz * l2],
                  dtype=float)

    return fl

  def lf(self, fx = 0.0, fz = 0.0):
    # load vector in LCS
    fl = self.lf_lcs(fx, fz)
    # transformation matrix
    t = self.t
    # load vector in GCS
    fe = t.T @ fl

    return fe

  def lt_lcs(self, t = 0.0, t0 = 0.0):
    EA = self.E * self.A
    # load vector in LCS
    ftl = np.array([-EA * a * (t - t0),
                    0.0,
                    0.0,
                    EA * a * (t - t0),
                    0.0,
                    0.0],
                   dtype=float)

    return ftl

  def lt(self, t = 0.0, t0 = 0.0):
    # load vector in LCS
    lt_lcs = self.lt_lcs(t, t0)
    # transformation matrix
    t = self.t
    # load vector in GCS
    fte = t.T @ lt_lcs

    return fte

  def ksig_lcs(self, N = 0.0):
    l = self.l
    l2 = l * l
    # initial stress matrix in LCS
    kl = (N / l) * np.array([[0.0, 0.0, 0.0, 0.0, 0.0, 0.0],
                             [0.0, 6.0 / 5.0, -l / 10.0, 0.0, -6.0 / 5.0, -l / 10.0],
                             [0.0, -l / 10.0, 2.0 * l2 / 15.0, 0.0 , l / 10.0, -l2 / 30.0],
                             [0.0, 0.0, 0.0, 0.0, 0.0, 0.0],
                             [0.0, -6.0 / 5.0, l / 10.0, 0.0, 6.0 / 5.0, l / 10.0],
                             [0.0, -l / 10.0, -l2 / 30.0, 0.0 , l / 10.0, 2.0 * l2 / 15.0]],
                            dtype=float)
    kl[0][0] = min(abs(kl[1][1]), abs(kl[2][2])) / 1000.0
    kl[0][3] = -kl[0][0]
    kl[3][0] = kl[0][3]
    kl[3][3] = kl[0][0]

    return kl

  def ksig(self, N = 0.0):
    # initial stress matrix in LCS
    ksig_lcs = self.ksig_lcs(N)
    # transformation matrix
    t = self.t
    # initial stress matrix in GCS
    # ksig = t.T.dot(kl.dot(t))
    ksig = t.T @ ksig_lcs @ t

    return ksig

  def postpro(self, ue: np.ndarray):
    '''
    ue = vector of nodal displacements of the element [1, 6]
    '''
    # local element stiffness matrix
    kl = self.k_lcs
    # transformation matrix
    t = self.t

    # element inner forces
    # se = kl.dot(t.dot(u))
    se = kl @ t @ ue

    return se


class Elements:
  def __add(self, element):
    if element is None:
      pass
    if type(element) is not Beam2D:
      raise WrongType(f'Element is not of type Beam2D, but {type(element)} - {str(element)}.')
    if element.id not in self.__items:
      raise UsedIndex(f'Element ID {element.id} already exists.')
    self.__items.setdefault(element.id, element)
    self.__count += 1

  def __init__(self):
    self.__count = 0
    self.__items = dict()

    self.__initialised = False

  def id(self, id):
    if id in self.__items:
      return self.__items[id]
    else:
      return False

  def add(self, element = None):
    self.__add(element)

  @property
  def count(self):
    return self.__count

  def init(self, nodes, props, mats):
    for i in self.__items:
      i.init(nodes, props, mats):
    self.__initialised = True


class Constraint:
  def __init__(self, id, ndid, tx = 0.0, ty = 0.0, tz = 0.0, rx = 0.0, ry = 0.0, rz = 0.0, label = None):
    self.__id = id
    self.__nd = ndid
    self.__prescribed = list(tx, ty, tz, rx, ry, rz)











