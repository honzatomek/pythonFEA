from templates.errors import *
from templates.basic import Basic
import defaults

import numpy as np

class Node2D(Basic):
  command = 'COOR2D'
  type = 'Node2D'

  def __init__(self, id, x, y, label = None):
    super().__init__(id=id, label=label)
    self.x = x
    self.y = y

  def __str__(self):
    if self.label is not None:
      return f'{self.id:8n} : {"".join([" {0:15.4f}".format(x) for x in self.coors])} : \'{self.label:s}\''
    else:
      return f'{self.id:8n} : {"".join([" {0:15.4f}".format(x) for x in self.coors])} '

  @property
  def coors(self):
    return np.asarray([self.x, self.y], dtype=defaults.DEFAULT_FLOAT)

  @coors.setter
  def coors(self, coors):
    if type(coors) in (list, tuple, np.ndarray):
      if type(coors) is np.ndarray:
        coors = coors.flatten()
        coors = coors.astype(defaults.DEFAULT_FLOAT)
      else:
        coors = np.asarray(coors, dtype=defaults.DEFAULT_FLOAT)
      if len(coors) != 2:
        raise MissingCoordinate(f'{self.type:s} ID {self.id:n}: Coordinate is missing, len(coors) = {len(coors):n} ({str(coors)}).')
      else:
        self.x = coors[0]
        self.y = coors[1]
    else:
      raise WrongType(f'{self.type:s} ID {self.id:n}: Coordinates are not iterable ({type(coors).__name__:s} != list, tuple, np.ndarray).')

  @property
  def x(self):
    return self.__x

  @x.setter
  def x(self, x):
    self.__x = defaults.DEFAULT_FLOAT(x)

  @property
  def y(self):
    return self.__y

  @y.setter
  def y(self, y):
    self.__y = defaults.DEFAULT_FLOAT(y)


class Node(Node2D):
  command = 'COOR'
  type = 'Node'

  def __init__(self, id, x, y, z, label = None):
    super().__init__(id=id, x=x, y=y, label=label)
    self.z = z

  def __str__(self):
    if self.label is not None:
      return f'{self.id:8n} : {"".join([" {0:15.4f}".format(x) for x in self.coors])} : \'{self.label:s}\''
    else:
      return f'{self.id:8n} : {"".join([" {0:15.4f}".format(x) for x in self.coors])} '

  @property
  def coors(self):
    return np.asarray([self.x, self.y, self.z], dtype=defaults.DEFAULT_FLOAT)

  @coors.setter
  def coors(self, coors):
    if type(coors) in (list, tuple, np.ndarray):
      if type(coors) is np.ndarray:
        coors = coors.flatten()
        coors = coors.astype(defaults.DEFAULT_FLOAT)
      else:
        coors = np.asarray(coors, dtype=defaults.DEFAULT_FLOAT)
      if len(coors) != 3:
        raise MissingCoordinate(f'{self.type:s} ID {self.id:n}: Coordinate is missing, len(coors) = {len(coors):n} ({str(coors)}).')
      else:
        self.x = coors[0]
        self.y = coors[1]
        self.z = coors[2]
    else:
      raise WrongType(f'{self.type:s} ID {self.id:n}: Coordinates are not iterable ({type(coors).__name__:s} != list, tuple, np.ndarray).')


  @property
  def z(self):
    return self.__z

  @z.setter
  def z(self, z):
    self.__z = defaults.DEFAULT_FLOAT(z)

