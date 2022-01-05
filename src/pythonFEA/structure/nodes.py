import defaults
from templates.errors import *
from templates.basic import Collection
from .node import Node2D, Node
import math

class Nodes(Collection):
  command = 'COORS'
  type = 'Nodes'
  member_types = [Node2D, Node]

  def __init__(self, label = None):
    super().__init__(label=label)
    self.__node_type = None

  @property
  def node_type(self):
    return self.__node_type

  def add(self, node):
    first = True if self.count == 0 else False
    nodes = node if type(node) in (list, tuple) else [node]
    for n in nodes:
      if not first and type(n) is not self.__node_type:
        raise WrongType(f'{type(self).type:s} cannot add {type(n).__name__:s}, type conflict (not {self.node_type.__name__:s})')
      super().add(n)
      if first:
        self.__node_type = type(n)
        first = False

  def distance(self, ndid1, ndid2):
    n1 = self.id(ndid1)
    n2 = self.id(ndid2)
    if type(n1) is not type(n2):
      raise WrongType(f'{repr(n1):s} is of different type to {repr(n2):s}.')
    n1c = n1.coors
    n2c = n2.coors
    if len(n1c) != len(n2c):
      raise NotValidCoor(f'{repr(n1):s} has different number of coordinates than {repr(n2):s} ({len(n1c):n} != {len(n2c):n}.')
    dist = 0
    for i in range(len(n1c)):
      dist += (n2c[i] - n1c[i]) ** 2
    dist = math.sqrt(dist)
    return dist
