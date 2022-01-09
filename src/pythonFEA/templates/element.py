import logging
import defaults
from .basic import Basic
from templates.material import Material
from .errors import *

class Element(Basic):
  command = 'ELEMENT'
  type = 'Element'
  material_types = [Material]
  nodes_number = 1
  storage_type = 'id'

  def __init__(self, id : int, mat: int, nodes: list, label: str = None):
    super().__init__(id=id, label=label)
    self.__init_node = False
    self.__init_mat = False
    self.mat = mat
    self.nodeids = nodes

  @property
  def mat(self):
    if not self.__init_mat:
      raise NotInitialised(f'{repr(self):s} cannot return Material as it was not yet linked.')
    else:
      return self.__material

  @property
  def matname(self):
    if self.__init_mat:
      return self.__material.label
    else:
      return self.__material

  @mat.setter
  def mat(self, material):
    if type(material) is str:
      self.__material = material
      self.__init_mat = False
    # elif type(material).__name__ not in selt.material_types: # isinstance()
    #   raise InvalidMaterial(f'{repr(self):s}: {repr(material):s} cannot be linked, must be one of ({", ".join(["{0}".format(m) for m in self.material_types])}).')
    else:
      self.__material = material
      self.__init_mat = True

  @property
  def nodeids(self):
    if not self.__init_node:
      return self.__nodes
    else:
      return [n.id for n in self.__nodes]

  @nodeids.setter
  def nodeids(self, nodes):
    n = nodes if type(nodes) in (list, tuple) else [nodes]
    if len(n) != self.nodes_number:
      raise NodeMissing(f'{repr(self):s} Nodes number must be {self.nodes_number:n}, not {len(n):n}.')
    else:
      for nd in n:
        if type(nd) is not int:
          raise WrongType(f'{repr(self):s} Node ID must be an int, not {type(mid).__name__} ({mid}).')
      self.__nodes = n
      self.__init_node = False

  @property
  def nodes(self):
    if not self.__init_node:
      raise NotInitialised(f'{repr(self):s} cannot return Nodes as they were not yet linked.')
    else:
      return self.__nodes

  @nodes.setter
  def nodes(self, nodes):
    nds = nodes if type(nodes) in (list, tuple) else [nodes]
    if len(nds) != len(self.__nodes):
      raise NodeMissing(f'{repr(self):s}: Number of nodes must be equal to number of Node IDs ({len(nds):n} != {len(self.__nodes):n}.')
    else:
      nds = {n.id: n for n in nds}
      for i in range(len(self.__nodes)):
        if self.__nodes[i] not in nds.keys():
          raise MissingIndex(f'{repr(self):s}: Node ID {self.__nodes[i]:n} is missing (Node IDs supplied: [{", ".join(["{0:n}".format(n.id) for n in nds.values()])}]).')
        else:
          self.__nodes[i] = nds[self.__nodes[i]]
      self.__init_node = True

  def link_mat(self, materials):
    self.mat = materials.id(self.matname)

  def link_nodes(self, nodes):
    for i in range(len(self.__nodes)):
      self.__nodes[i] = nodes.id(self.__nodes[i])
    self.__init_node = True

  def link(self, nodes, materials):
    self.link_nodes(nodes)
    self.link_mat(materials)

  @property
  def linked_nodes(self):
    return self.__init_node

  @property
  def linked_mat(self):
    return self.__init_mat

  @property
  def consolidated(self):
    return self.linked_nodes and self.linked_mat

  @property
  def t_gcs2lcs(self):
    if not self.linked_nodes:
      raise NotLinked(f'{repr(self):s}: Nodes have not yet been linked.')
    raise NotImplemented(f'{type(self).__name__}.t_gcs2lcs')

  @property
  def t_lcs2gcs(self):
    return self.t_gcs2lcs.T

  @property
  def ke(self):
    if not self.linked_nodes:
      raise NotLinked(f'{repr(self):s}: Nodes have not yet been linked.')
    if not self.linked_mat:
      raise NotLinked(f'{repr(self):s}: Material has not yet been linked.')
    raise NotImplemented(f'{type(self).__name__}.ke')

  def me(self):
    if not self.linked_nodes:
      raise NotLinked(f'{repr(self):s}: Nodes have not yet been linked.')
    if not self.linked_mat:
      raise NotLinked(f'{repr(self):s}: Material has not yet been linked.')
    raise NotImplemented(f'{type(self).__name__}.me')

  def lf(self):
    '''
    force load
    '''
    if not self.linked_nodes:
      raise NotLinked(f'{repr(self):s}: Nodes have not yet been linked.')
    raise NotImplemented(f'{type(self).__name__}.le')

  def lt(self):
    '''
    thermal load
    '''
    if not self.linked_nodes:
      raise NotLinked(f'{repr(self):s}: Nodes have not yet been linked.')
    raise NotImplemented(f'{type(self).__name__}.le')

  def ksig(self):
    if not self.linked_nodes:
      raise NotLinked(f'{repr(self):s}: Nodes have not yet been linked.')
    if not self.linked_mat:
      raise NotLinked(f'{repr(self):s}: Material has not yet been linked.')
    raise NotImplemented(f'{type(self).__name__}.ksig')

  def postpro(self):
    if not self.linked_nodes:
      raise NotLinked(f'{repr(self):s}: Nodes have not yet been linked.')
    if not self.linked_mat:
      raise NotLinked(f'{repr(self):s}: Material has not yet been linked.')
    raise NotImplemented(f'{type(self).__name__}.postpro')


