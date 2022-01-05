import math
import numpy as np
import logging
# from errors.errors import *

BASIC_FORMAT = logging.Formatter('%(message)s')
ADVANCED_FORMAT = logging.Formatter('%(asctime)6s *%(levelname).1s* %(message)s', datefmt='%H%M%S')

logging.basicConfig(level=logging.INFO, format='%(asctime)6s *%(levelname).1s* %(message)s', datefmt='%H%M%S')



class Node:
  COMMAND = '$COOR'

  def __init__(self, id, x, y, z, label = None):
    self.__id = id
    self.__x = x
    self.__y = y
    self.__z = z
    self.__label = label

  def __str__(self):
    if self.label:
      return f'    {self.id:8n} : {self.x:15.4f} {self.y:15.4f} {self.z:15.4f} : \'{self.label}\''
    else:
      return f'    {self.id:8n} : {self.x:15.4f} {self.y:15.4f} {self.z:15.4f}'

  def __repr__(self):
    if self.label:
      return f'Node {self.id:n} - {self.label:s}'
    else:
      return f'Node {self.id:n}'

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
    if node.id in self.__items.keys():
      raise UsedIndex(f'Node ID {node.id} already exists.')
    self.__items.setdefault(node.id, node)
    self.__count += 1

  def __init__(self):
    self.__count = 0
    self.__items = dict()

  def __str__(self):
    nodes = ''
    command = ''
    for i, n in self.__items.items():
      if n.COMMAND != command:
        nodes += '\n' if command != '' else ''
        command = n.COMMAND
        nodes += '  ' + command + '\n'
      nodes += str(n) + '\n'
    return nodes

  def __repr__(self):
    return f'Nodes count: {self.count}, min ID: {self.min}, max ID: {self.max}'

  @property
  def min(self):
    return math.min(self.__items.keys())

  @property
  def max(self):
    return math.max(self.__items.keys())

  def id(self, id):
    if id in self.__items:
      return self.__items[id]
    else:
      return False

  def add(self, node):
    self.__add(node)

  @property
  def count(self):
    return self.__count

  def distance(self, id1, id2):
    n1 = self.id(id1)
    n2 = self.id(id2)
    return math.sqrt((n2.x - n1.x) ** 2 + (n2.y - n1.y) ** 2 + (n2.z - n1.z) ** 2)


class Beam2D:
  COMMAND = '$BEAM2D'

  def __init__(self, id, pid, mid, nid1, nid2, rx1 = False, rz1 = False, rfi1 = False, rx2 = False, rz2 = False, rfi2 = False, label = None, mi = 0.5):
    self.__id = id
    self.__n1 = int(nid1)
    self.__r1 = [rx1, rz1, rfi1]
    self.__n2 = int(nid2)
    self.__r2 = [rx2, rz2, rfi2]
    self.__p = int(pid)
    self.__m = int(mid)
    self.__label = label

    self.__length = None
    self.__loc = None

    # ratio between lumped and consistent mass matrix
    self.__mi = 0.5

    self.__nodes_ref = False
    self.__prop_ref = False
    self.__mat_ref = False

  def __str__(self):
    if self.label:
      return f'    {self.id:8n} : {self.pid:8n} {self.mid:8n} : {self.n1id:8n} {self.n2id:8n}\n    &        : {"".join(["{0:4n}".format(int(r)) for r in self.r1])}  {"".join(["{0:4n}".format(int(r)) for r in self.r1])} : \'{self.label}\''
    else:
      return f'    {self.id:8n} : {self.pid:8n} {self.mid:8n} : {self.n1id:8n} {self.n2id:8n}\n    &        : {"".join(["{0:4n}".format(int(r)) for r in self.r1])}  {"".join(["{0:4n}".format(int(r)) for r in self.r1])}'

  def __repr__(self):
    if self.label:
      return f'{type(self)} {self.id:n} - {self.label:s}'
    else:
      return f'{type(self)} {self.id:n}'

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
    self.__nodes_ref = False

  @property
  def n1id(self):
    if self.initialised:
      return self.__n1.id
    else:
      return self.__n1

  @property
  def n2(self):
    return self.__n2

  @n2.setter
  def n2(self, n2):
    self.__n2 = n2
    self.__nodes_ref = False

  @property
  def n2id(self):
    if self.initialised:
      return self.__n2.id
    else:
      return self.__n2

  @property
  def r1(self):
    return self.__r1

  @property
  def r2(self):
    return self.__r2

  @property
  def prop(self):
    if self.initialised:
      return self.__p
    else:
      return None

  @prop.setter
  def prop(self, prop):
    self.__p = prop

  @property
  def pid(self):
    if self.initialised:
      return self.__p.id
    else:
      return self.__p

  @pid.setter
  def pid(self, pid):
    self.__p = pid
    self.__prop_ref = False

  @property
  def mat(self):
    if self.initialised:
      return self._m
    else:
      return None

  @property
  def mid(self):
    if self.initialised:
      return self.__m.id
    else:
      return self.__m

  @mid.setter
  def mid(self, mid):
    self.__m = mid
    self.__mat_ref = False

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
    return self.__llength

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
    '''
    transformation matrix GCS -> LCS
    '''
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
  def m_gcs(self):
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
    if element.id in self.__items.keys():
      raise UsedIndex(f'Element ID {element.id} already exists.')
    self.__items.setdefault(element.id, element)
    self.__count += 1

  def __init__(self):
    self.__count = 0
    self.__items = dict()

    self.__initialised = False

  def __str__(self):
    elements = ''
    command = ''
    for i, e in self.__items.items():
      if e.COMMAND != command:
        elements += '\n' if command != '' else ''
        command = e.COMMAND
        elements += '  ' + command + '\n'
      elements += str(e) + '\n'
    return elements

  def __repr__(self):
    return f'Elements count: {self.count}, min ID: {self.min}, max ID: {self.max}'

  @property
  def min(self):
    return math.min(self.__items.keys())

  @property
  def max(self):
    return math.max(self.__items.keys())

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
    for id, el in self.__items.items():
      el.init(nodes, props, mats)
    self.__initialised = True

  @property
  def initialised(self):
    if not self.__initialised:
      return False
    for id, el in self.__items.items():
      if not el.initialised:
        self.__initialised = False
        return False
    self.__initialised = True
    return True


class ConstraintNodal:
  COMMAND = '$PRESCRIBE'

  def __init__(self, id, lpat, ndid, dofs, prescribed = None, label = None):
    self.__id = id
    self.__lpat = lpat
    self.__nd = ndid
    if prescribed is None:
      prescribed = [0.0 for d in dofs]
    if len(dofs) != len(prescribed):
      raise ConstraintDOFs(f'Constraint ID {self.__id} DOFs number does not match prescribed values ({str(dofs)} != {str(prescribed)}).')
    self.__dofs = dofs
    self.__prescribed = prescribed
    self.__label = label

    self.__loc = None

    self.__initialised = False

  def __str__(self):
    if self.label:
      retval = f'  {self.COMMAND} LPAT = {self.lpat} DOF = {"".join(["{0:n}".format(d) for d in self.__dofs])} NAME = \'{self.label}\'\n'
    else:
      retval = f'  {self.COMMAND} LPAT = {self.lpat} DOF = {"".join(["{0:n}".format(d) for d in self.__dofs])}\n'
    retval += f'      {self.ndid:8n} : {" ".join(["{0:16.8f}".format(p) for p in self.__prescribed])}\n'

    return retval

  def __repr__(self):
    return 'Nodal Constraint {self.id} - {len(self.__nd)} nodes, dofs = {"".join(["{0:n}".format(d) for self.__dofs])}'

  def init(self, nodes):
    n = nodes.id(self.__nd)
    if n:
      self.__nd = n
    else:
      raise MissingIndex(f'Node ID {self.__nd} is missing.')
    self.__initialised = True

  @property
  def id(self):
    return self.__id

  @id.setter
  def id(self, id):
    self.__id = id

  @property
  def lpat(self):
    return self.__lpat

  @lpat.setter
  def lpat(self, lpat):
    self.__lpat = lpat

  @property
  def nd(self):
    return self.__nd

  @nd.setter
  def nd(self, node):
    self.__nd = node
    if type(node) is Node:
      self.__initialised = True
    else:
      self.__initialised = False

  @property
  def label(self):
    return self.__label

  @label.setter
  def label(self, label):
    self.__label = label

  @property
  def prescribe(self):
    return self.__prescribed

  @prescribe.setter
  def prescribe(self, t1 = 0.0, t2 = 0.0, t3 = 0.0, r1 = 0.0, r2 = 0.0, r3 = 0.0):
    self.__prescribed = np.asarray([t1, t2, t3, r1, r2, r3], dtype=float)

  @property
  def initialised(self):
    return self.__initialised


class Constraints:
  COMMAD = '$CONSTRAINTS'

  def __add(self, constraint):
    if constraint is None:
      pass
    if type(constraint) is not ConstraintNodal:
      raise WrongType(f'Constraint is not of type ConstraintNodal, but {type(constraint)} - {str(constraint)}.')
    if constraint.id not in self.__items:
      raise UsedIndex(f'Constraint ID {constraint.id} already exists.')
    self.__items.setdefault(constraint.id, constraint)
    self.__count += 1

  def __init__(self, label):
    self.__count = 0
    self.__items = dict()

    self.__label = label

    self.__initialised = False

  def __str__(self):
    pass

  def __repr__(self):
    pass

  @property
  def label(self):
    return self.__label

  @label.setter
  def label(self, label):
    self.__label = label

  def id(self, id):
    if id in self.__items:
      return self.__items[id]
    else:
      return False

  def add(self, constraint = None):
    self.__add(constraint)

  @property
  def count(self):
    return self.__count

  def init(self, nodes):
    for id, c in self.__items.items():
      c.init(nodes)
    self.__initialised = True

  @property
  def initialised(self):
    if not self.__initialised:
      return False
    for id, c in self.__items.items():
      if not c.initialised:
        self.__initialised = False
        return False
    self.__initialised = True
    return True


class LoadNodal:
  type = 'node'

  def __init__(self, id, lpat, ndid, F1 = 0.0, F2 = 0.0, F3 = 0.0, M1 = 0.0, M2 = 0.0, M3 = 0.0, label = None):
    self.__id = id
    self.__lpat = lpat
    self.__nd = ndid
    self.__forces = np.asarray([F1, F2, F3, M1, M2, M3], dtype=float)

    self.__initialised = False

  def init(self, nodes):
    n = nodes.id(self.__nd)
    if n:
      self.__nd = n
    else:
      raise MissingIndex(f'Node ID {self.__nd} is missing.')
    self.__initialised = True

  @property
  def load_type(self):
    return self.type

  @property
  def id(self):
    return self.__id

  @id.setter
  def id(self, id):
    self.__id = id

  @property
  def lpat(self):
    return self.__lpat

  @lpat.setter
  def lpat(self, lpat):
    self.__lpat = lpat

  @property
  def nd(self):
    return self.__nd

  @nd.setter
  def nd(self, node):
    self.__nd = node
    if type(nd) is Node:
      self.__initialised = True
    else:
      self.__initialised = False

  @property
  def label(self):
    return self.__label

  @label.setter
  def label(self, label):
    self.__label = label

  @property
  def F1(self):
    return self.__forces[0]

  @F1.setter
  def F1(self, F1):
    self.__forces[0] = F1

  @property
  def F2(self):
    return self.__forces[1]

  @F2.setter
  def F2(self, F2):
    self.__forces[1] = F2

  @property
  def F3(self):
    return self.__forces[2]

  @F3.setter
  def F3(self, F3):
    self.__forces[2] = F3

  @property
  def M1(self):
    return self.__forces[3]

  @M1.setter
  def M1(self, M1):
    self.__forces[3] = M1

  @property
  def M2(self):
    return self.__forces[4]

  @M2.setter
  def M2(self, M2):
    self.__forces[4] = M2

  @property
  def M3(self):
    return self.__forces[5]

  @M3.setter
  def M3(self, M3):
    self.__forces[5] = M3

  @property
  def forces(self):
    return self.__forces

  @forces.setter
  def forces(self, F1 = 0.0, F2 = 0.0, F3 = 0.0, M1 = 0.0, M2 = 0.0, M3 = 0.0):
    self.__forces = np.asarray([F1, F2, F3, M1, M2, M3], dtype=float)


class LoadBeam2D:
  type = 'element'

  def __2gcs(self):
    if self.__dir == 'gcs':
      pass
    else:
      self.__forces = self.t @ self.__forces
      self.__dir = 'gcs'

  def __2lcs(self):
    if self.__dir == 'element':
      pass
    else:
      self.__forces = self.t.T @ self.__forces
      self.__dir = 'element'

  def __init__(self, id, lpat, eid, fx = 0.0, fz = 0.0, dir = 'element', label = None):
    '''
    dir = element/gcs
    '''
    self.__id = id
    self.__lpat = lpat
    self.__el = eid
    self.__dir = dir
    self.__forces = np.asarray([[fx], [fz]], dtype=float)

    self.__label = label

    self.__initialised = False

  def init(self, elements):
    e = elements.id(self.__el)
    if e:
      self.el = e
    else:
      raise MissingIndex(f'Element ID {self.__el} is missing.')
    self.__2lcs()
    self.__initialised = True

  @property
  def load_type(self):
    return self.type

  @property
  def id(self):
    return self.__id

  @id.setter
  def id(self, id):
    self.__id = id

  @property
  def lpat(self):
    return self.__lpat

  @lpat.setter
  def lpat(self, lpat):
    self.__lpat = lpat

  @property
  def el(self):
    return self.__el

  @el.setter
  def el(self, element):
    if type(element) is not int or type(element) is not Beam2D:
      raise WrongType(f'Element Type for Load ID {self.__id} is not Beam2D, but {type(element)}.')
    self.__el = element
    if type(element) is Beam2D:
      self.__initialised = True
    else:
      self.__initialised = False

  @property
  def label(self):
    return self.__label

  @label.setter
  def label(self, label):
    self.__label = label

  @property
  def fx(self):
    return self.__forces[0]

  @fx.setter
  def fx(self, fx):
    self.__forces[0] = fx

  @property
  def fz(self):
    return self.__forces[1]

  @fz.setter
  def fz(self, fz):
    self.__forces[1] = fz

  @property
  def forces(self):
    return self.__forces

  @forces.setter
  def forces(self, fx = 0.0, fz = 0.0, dir = 'element'):
    '''
    dir = element/gcs
    '''
    self.__dir = dir
    self.__forces = np.asarray([[fx], [fz]], dtype=float)

  @property
  def t(self):
    if self.__initialised:
      # transformation GCS -> LCS
      t = self.__el.t

      return t[0:2, 0:2]


class Loads:
  def __add(self, load):
    if load is None:
      pass
    if type(load) is not LoadNodal or type(load) is not LoadBeam2D:
      raise WrongType(f'Load is not of type Load, but {type(load)} - {str(load)}.')
    if load.id not in self.__items:
      raise UsedIndex(f'Load ID {load.id} already exists.')
    self.__items.setdefault(load.id, load)
    self.__lpat.setdefault(load.lpat, [])
    self.__lpat[load.lpat].append(load)
    self.__count += 1

  def __init__(self):
    self.__count = 0
    self.__items = dict()
    self.__lpat = dict()

    self.__initialised = False

  def id(self, id):
    if id in self.__items:
      return self.__items[id]
    else:
      return False

  def lpats(self):
    return list(self.__lpat.keys())

  def lpat(self, lpatid):
    # lpatloads = list()
    # for id, l in self.__items.items():
    #   if l.lpat == lpatid:
    #     lpatloads.append(l)
    # return lpatloads
    return list(self.__lpat.values())


  def add(self, load = None):
    self.__add(load)

  @property
  def count(self):
    return self.__count

  def init(self, nodes, elements):
    for id, l in self.__items.items():
      if l.type == 'node':
        l.init(nodes)
      elif l.type == 'element':
        l.init(elements)
    return self.initialised

  @property
  def initialised(self):
    if not self.__initialised:
      return False
    for id, l in self.__items.items():
      if not l.initialised:
        self.__initialised = False
        return False
    self.__initialised = True
    return True


class PropertyBeam2D:
  def __init__(self, id, A, I, nsm = 0.0, label = None):
    self.__id = id
    self.__A = A
    self.__I = I
    self.__nsm = nsm
    self.__label = label

  @property
  def id(self):
    return self.__id

  @id.setter
  def id(self, id):
    self.__id = id

  @property
  def A(self):
    return self.__A

  @A.setter
  def A(self, A):
    self.__A = A

  @property
  def I(self):
    return self.__I

  @I.setter
  def I(self, I):
    self.__I = I

  @property
  def nsm(self):
    return self.__nsm

  @nsm.setter
  def nsm(self, nsm):
    self.__nsm = nsm

  @property
  def label(self):
    return self.__label

  @label.setter
  def label(self, label):
    self.__label = label


class Properties:
  def __add(self, prop):
    if prop is None:
      pass
    if type(prop) is not PropertyBeam2D:
      raise WrongType(f'Property is not of type PropertyBeam2D, but {type(prop)} - {str(prop)}.')
    if prop.id not in self.__items:
      raise UsedIndex(f'Property ID {prop.id} already exists.')
    self.__items.setdefault(prop.id, prop)
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

  def add(self, prop = None):
    self.__add(prop)


class MaterialLinear2D:
  def __init__(self, id, E, ro, alpha, label = None):
    self.__id = id
    self.__E = E
    self.__ro = ro
    self.__alpha = alpha
    self.__label = label

  @property
  def id(self):
    return self.__id

  @id.setter
  def id(self, id):
    self.__id = id

  @property
  def E(self):
    return self.__E

  @E.setter
  def E(self, E):
    self.__E = E

  @property
  def ro(self):
    return self.__ro

  @ro.setter
  def ro(self, ro):
    self.__ro = ro

  @property
  def alpha(self):
    return self.__alpha

  @alpha.setter
  def alpha(self, alpha):
    self.__alpha = alpha

  @property
  def label(self):
    return self.__label

  @label.setter
  def label(self, label):
    self.__label = label


class Materials:
  def __add(self, mat):
    if mat is None:
      pass
    if type(mat) is not MaterialLinear2D:
      raise WrongType(f'Material is not of type MaterialLinear2D, but {type(mat)} - {str(mat)}.')
    if mat.id not in self.__items:
      raise UsedIndex(f'Material ID {mat.id} already exists.')
    self.__items.setdefault(mat.id, mat)
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

  def add(self, mat = None):
    self.__add(mat)


class Structure:
  def __init__(self, label):
    self.__label = label
    self.__structure = dict()
    self.__nodes = Nodes()
    self.__elements = Elements()

  def new(self, situation, constraints = None, loads = None, props = None, mats = None):
    self.__structure.setdefault(situation, dict())
    self.__structure[situation]['constraints'] = constraints if constraints else Constraints()
    self.__structure[situation]['loads'] = loads if loads else Loads()
    self.__structure[situation]['properties'] = props if props else Properties()
    self.__structure[situation]['materials'] = mats if mats else Materials()

  @property
  def label(self):
    return self.__label

  @label.setter
  def label(self, label):
    self.__label = label

  @property
  def nodes(self):
    return self.__nodes

  @property
  def elements(self):
    return self.__elements

  def constraints(self, situation):
    return self.__structure[situation]['constraints']

  def loads(self, situation):
    return self.__structure[situation]['loads']

  def properties(self, situation):
    return self.__structure[situation]['properties']

  def materials(self, situation):
    return self.__structure[situation]['materials']

  def init(self, situation = None):
    if situation and situation not in self.__structure.keys():
      raise SituationMissing(f'Structure {self.label} does not contain Situation {situation}.')
    sit = [situation] if situation else list(self.__structure.keys())

    for s in sit:
      self.elements.init(self.__nodes, self.properties(s), self.materials(s))
      self.constraints(s).init(self.__nodes)
      self.loads(s).init(self._nodes, self.__elements)

  @property
  def check(self):
    # check nodes
    if self.nodes.count == 0:
      raise NodeMissing(f'Structure {self.__label} does not contain any Nodes.')

    # check elements
    if self.elements.count == 0:
      raise ElementMissing(f'Structure {self.__label} does not contain any Elements.')

    for sit in self.__structure.keys():
      # check loads
      if self.__structure[sit]['loads'].count == 0:
        raise LoadMissing(f'Structure {self.__label} does not contain any Loads in Situation {sit}.')

      # check properties
      if self.__structure[sit]['properties'].count == 0:
        raise PropertyMissing(f'Structure {self.__label} does not contain any Properties in Situation {sit}.')

      # check materials
      if self.__structure[sit]['materials'].count == 0:
        raise PropertyMissing(f'Structure {self.__label} does not contain any Materials in Situation {sit}.')




