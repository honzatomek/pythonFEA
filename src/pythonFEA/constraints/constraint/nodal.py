import logging

from templates.basic import Basic, Collection
from templates.errors import *

class Suppress:
  command = 'SUPPRESS'

  def __init__(self, dofs, nodes = None, label = None):
    self.__dofs = []
    self.__unpacked = False
    self.dofs = dofs
    if nodes is not None:
      self.add(dofs, nodes)
    else:
      self.nodes = []
      self.nodesets = []

  def __str__(self):
    out = ''
    out += f'${type(self).command:s} DOFS = \'{",".join([str(d) for d in self.dofs])}\'\n'
    nids = ''
    if len(self.nodesets) > 0:
      for i, nodeset in enumerate(self.nodesets):
        if ' ' in nodeset:
          out += '  \'' + nodeset + '\'\n'
        else:
          out += '  ' + nodeset + '\n'
    if len(self.nodes) > 0:
      for i, node in enumerate(self.nodes):
        if i % 6 == 0 and i != 0:
          out += '  ' + nids + '\n'
          nids = ''
        nids += f'  {node:10n}'
      if nids != '':
        out += '  ' + nids + '\n'
    return out[:-2]

  def __repr__(self):
    return f'{type(self).__name__:s} dofs: {",".join([str(s) for d in self.dofs]):s}, nodes: {len(self.nodes):n}, nodesets: {len(self.nodesets:n}'

  def __iter__(self):
    return iter(self.items())

  def __len__(self):
    return len(self.items())

  def items(self):
    if self.unpacked:
      return self.__nodes
    else:
      nds = list()
      if len(self.__nodes) > 0:
        nds.extend(self.__nodes)
      if len(self.__nodesets) > 0:
        nds.extend(self.__nodesets)
      return nds

  @property
  def unpacked(self):
    return self.__unpacked

  @property
  def dofs(self):
    return self.__dofs

  @dofs.setter
  def dofs(self, dofs):
    if type(dofs) is str:
      self.__dofs = [int(d) for d in dofs.split(',')]
    elif type(dofs) is int:
      self.__dofs = dofs
    elif type(dofs) in [tuple, list]
      self.__dofs.extend([int(d) for d in dofs])

  def add(self, nodes):
    if type(nodes) in [int, str]:
      nodelist = [nodes]
    else:
      nodelist = nodes
    nds = []
    sets = []
    for n in nodelist:
      (nds if type(n) is int else sets).append(n)
    self.nodes = nds
    self.nodesets = sets

  @property
  def nodes(self):
    return self.__nodes

  @nodes.setter
  def nodes(self, nodes):
    if type(nodes) in [list, tuple]:
      if len(nodes) > 0:
        self.__nodes.extend(nodes)
    else:
      self.__nodes.append(nodes)

  @property
  def nodesets(self):
    return self.__nodesets

  @nodesets.setter
  def nodesets(self, nodesets):
    if type(nodesets) in [list, tuple]:
      if len(nodesets) > 0:
        self.__nodesets.extend(nodesets)
    else:
      self.__nodesets.append(nodesets)

  def unpack_set(self, nsets):
    for i in range(len(self.__nodesets)):
      set = self.__nodesets.pop(-1)
      if set in nsets.keys():
        self.__nodes.append(nsets[set])
      else:
        raise MissingLabel(f'{type(self).__name__:s} Node Set {set:s} is missing!')
    self.__unpacked = True


class Prescribe(Suppress):
  command = 'PRESCRIBE'

