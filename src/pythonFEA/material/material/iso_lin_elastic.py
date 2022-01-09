import logging

import defaults
from templates.errors import *
from templates.material import Material


class LinearElastic(Material):
  command = 'MATERIAL'
  type = 'ISO'
  storage_type = 'label'

  def __init__(self, label, E, nu, ro = 0.0, alpha = 0.0):
    super().__init__(label=label)
    self.E = E
    self.nu = nu
    self.ro = ro
    self.alpha = alpha

  def __str__(self):
    out = f'${type(self).command:s} NAME = \'{self.label}\' TYPE = {type(self).type:s}\n'
    out += f'  $YOUNG TYPE = VALUE\n    {self.E:12.4E} {self.nu:12.4E}\n'
    out += f'  $DENSITY TYPE = VALUE\n    {self.ro:12.4E}\n'
    out += f'  $THERMAL TYPE = VALUE\n    {self.alpha:12.4E}\n'
    out += '$END MATERIAL'
    return out

  @property
  def E(self):
    return self.__E

  @E.setter
  def E(self, value):
    self.__E = value

  @property
  def nu(self):
    return self.__nu

  @nu.setter
  def nu(self, value):
    self.__nu = value

  @property
  def ro(self):
    return self.__ro

  @ro.setter
  def ro(self, value):
    self.__ro = value

  @property
  def alpha(self):
    return self.__alpha

  @alpha.setter
  def alpha(self, value):
    self.__alpha = value

