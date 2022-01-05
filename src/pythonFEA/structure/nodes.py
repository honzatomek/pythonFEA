import defaults
from templates.errors import *
from templates.basic import Collection
from .node import Node2D, Node
import math

class Nodes(Collection):
  command = 'COORS'
  type = 'Nodes'
  member_types = [Node2D, Node]

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
