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
    return f'{self.label:s}\n{self.A:12.4E} {self.Izz:12.4E} : {self.nsm:12.4E}'

  @property
  def A(self):
    return self.A

  @A.setter
  def A(self, A: float):
    self.__A = A

  @property
  def Izz(self):
    return self.Izz

  @Izz.setter
  def Izz(self, Izz: float):
    self.__Izz = Izz
