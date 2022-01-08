from .errors import *
from basic import Basic
import logging

class Property(Basic):
  command = 'PROPERTY'
  type = 'Property'
  storage_type = 'label'

  def __init__(self, label, non_structural_mass = 0.0):
    super().__init__(id=None, label=label):
    self.nsm = non_structural_mass

  @property
  def nsm(self):
    return self.__nsm

  @nsm.setter
  def nsm(self, non_structural_mass):
    self.__nsm = non_structural_mass

