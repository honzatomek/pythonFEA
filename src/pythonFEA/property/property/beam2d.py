import logging

import defaults
from templates.errors import *
from templates.property import Property

class PBeam2D(Property):
  command = 'PBEAM2D'
  type = 'PBeam2D'

  def __init__(self, label: str, A: float, Izz: float, nsm: float):
    super().__init__(label=label, non_structural_mass = nsm)
    self.A = A
    self.Izz = A

  def __str__(self):
    out = f'${type(self).command:s} NAME = \'{self.label:s}\'\n'
    out += f'  $SECTION TYPE = VALUE\n    {self.A:12.4E} {self.Izz:12.4E}\n'
    out += f'  $MASS TYPE = VALUE\n    {self.nsm:12.4E}\n'
    out += f'$END {type(self).command:s}'
    return out

  @property
  def A(self):
    return self.__A

  @A.setter
  def A(self, A: float):
    self.__A = A

  @property
  def Izz(self):
    return self.__Izz

  @Izz.setter
  def Izz(self, Izz: float):
    self.__Izz = Izz
