import defaults
from templates.element import Element
from templates.errors import *
import logging
import math
import numpy as np

class Beam2D(Element):
  command = 'BEAM2D'
  type = 'Beam2D'
  material_types = ('Material')
  property_types = ('PBeam2D')
  nodes_number = 2

  def __init__(self, id: int, mat: int, prop: int, nodes: list, releases: list = None, label: str = None):
    super().__init__(id=id, mat=mat, nodes=nodes, label=label)
    self.__init_prop = False
    self.prop = prop
    self.releases = releases

  @property
  def prop(self):
    if not self.__init_prop:
      raise NotInitialised(f'{repr(self):s} cannot return Property as it was not yet linked.')
    else:
      return self.__prop

  @property
  def propname(self):
    if self.__init_prop:
      return self.__property.label
    else:
      return self.__property

  @prop.setter
  def prop(self, property):
    if type(property) is str:
      self.__prop = property
      self.__init_prop = False
    elif type(property).__name__ not in self.property_types:
      raise InvalidProperty(f'{repr(self):s}: {repr(property):s} cannot be linked, must be one of ({", ".join(["{0}".format(m) for m in self.property_types])}).')
    else:
      self.__prop = property
      self.__init_prop = True

  @property
  def releases(self):
    return self.__releases

  @releases.setter
  def releases(self, releases = None):
    if releases is None:
      self.__releases = [[False, False, False] for i in range(self.nodes_number)]
    else:
      if len(releases) != self.nodes_number:
        raise ReleaseMissing(f'{repr(self):s} Releases do not match nodes number (2 sets required != {len(releases):n} provided).')
      for i in range(self.nodes_number):
        if len(releases[i]) != 3:
          raise ReleaseMissing(f'{repr(self):s} Number of releases for Node {i+1:n} must be 3 ({str(releases[i])}).')
      self.__releases = releases

  def link_prop(self, properties):
    self.prop = properties.id(self.prop)

  def link(self, nodes, materials, properties):
    super().link(nodes, materials)
    self.link_prop(properties)

  @property
  def linked_prop(self):
    return self.__init_prop

  @property
  def consolidated(self):
    return self.linked_prop and super().consolidated

  @property
  def length(self):
    l = 0
    nds = self.nodes
    n1 = nds[0].coors
    n2 = nds[1].coors
    d = 0
    for i in range(len(n1)):
      d += (n2[i] - n1[i]) ** 2
    d = math.sqrt(d)
    return d

  @property
  def t_gcs2lcs(self):
    '''
    transformation matrix GCS -> LCS
    '''
    n1 = self.nodes[0]
    n2 = self.nodes[1]
    c = (n2[0] - n1[0]) / self.length
    s = (n2[1] - n1[1]) / self.length

    t = np.array([[c, s, 0.0, 0.0, 0.0, 0.0],
                  [-s, c, 0.0, 0.0, 0.0, 0.0],
                  [0.0, 0.0, 1.0, 0.0, 0.0, 0.0],
                  [0.0, 0.0, 0.0, c, s, 0.0],
                  [0.0, 0.0, 0.0, -s, c, 0.0],
                  [0.0, 0.0, 0.0, 0.0, 0.0, 1.0]],
                 dtype=float)
    return t

  @property
  def ke_lcs(self):
    '''
    Beam stiffness matrix in LCS (Kirchhoff 2D)
    '''
    l = self.length
    A = self.prop.A
    I = self.prop.Izz
    E = self.mat.E

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
    '''
    Global Beam stiffness matrix
    '''
    # local element stiffness
    kl = self.ke_lcs
    # transformation matrix
    t = self.t_gcs2lcs
    # transformation LCS -> GCS
    ke = t.T @ kl @ t

    return ke

  @property
  def m_lumped(self):
    '''
    Lumped Mass matrix
    '''
    # structural mass
    sm = self.prop.A * self.length * self.mat.ro
    # nonstructural mass
    nm = self.length * self.prop.nsm
    # local lumped element mass matrix
    mle = np.eye(6, dtype=float) * (sm + nm) / 2
    mle[2][2] = 0.0
    mle[5][5] = 0.0

    return mle

  @property
  def m_consistent(self):
    '''
    Consistent Mass matrix
    '''
    l = self.length
    l2 = l ** l
    # structural mass
    sm = self.prop.A * l * self.mat.ro
    # nonstructural mass
    nm = l * self.prop.nsm
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

  def me(self, mi = 0.5):
    '''
    Mass matrix of Beam2D element as a combination of lumped and consistent mass matrix
    '''
    # get mass in LCS
    ml = (1.0 - self.mi) * self.m_consistent + self.mi * self.m_lumped
    # transformation matrix
    t = self.t_gcs2lcs

    # transformation LCS -> GCS
    me = t.T @ ml @ t

    return me

  def lf_lcs(self, fx = 0.0, fz = 0.0):
    l = self.length
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
    t = self.t_gcs2lcs
    # load vector in GCS
    fe = t.T @ fl

    return fe

  def lt_lcs(self, t = 0.0, t0 = 0.0):
    a = self.mat.alpha
    EA = self.mat.E * self.prop.A
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
    t = self.t_gcs2lcs
    # load vector in GCS
    fte = t.T @ lt_lcs

    return fte

  def ksig_lcs(self, N = 0.0):
    l = self.length
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
    t = self.t_gcs2lcs
    # initial stress matrix in GCS
    # ksig = t.T.dot(kl.dot(t))
    ksig = t.T @ ksig_lcs @ t

    return ksig

  def postpro(self, ue: np.ndarray):
    '''
    ue = vector of nodal displacements of the element [1, 6]
    '''
    # local element stiffness matrix
    kl = self.ke_lcs
    # transformation matrix
    t = self.t_gcs2lcs

    # element inner forces
    # se = kl.dot(t.dot(u))
    se = kl @ t @ ue

    return se


