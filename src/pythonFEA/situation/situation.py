import logging

from templates.basic import Basic
from templates.errors import *

class Situation(Basic):
  command = 'SITUATION'
  storage_type = 'label'

  def __init__(self, label, structure, loading, constraints, system, property, material):
    super().__init__(label=label)
    self.structure = structure
    self.loading = loading
    self.constraints = constraints
    self.system = system
    self.property = property
    self.material = material

  def __str__(self):
    out = f'${type(self).command:s} NAME = {self.label:s}\n'
    out += f'  STRUCTURE = {self.structure.label:s}\n'
    out += f'  LOADING = {self.loading.label:s}\n'
    out += f'  CONSTRAINTS = {self.constraints.label:s}\n'
    out += f'  SYSTEM = {self.system.label:s}\n'
    out += f'  PROPERTY = {self.property.label:s}\n'
    out += f'  MATERIAL = {self.material.label:s}\n'
    out += f'$END {type(self).command:s}'
    return out

  @property
  def structure(self):
    return self.__structure

  @structure.setter
  def structure(self, structure):
    self.__structure = structure

  @property
  def loading(self):
    return self.__loading

  @loading.setter
  def loading(self, loading):
    self.__loading = loading

  @property
  def constraints(self):
    return self.__constraints

  @constraints.setter
  def constraints(self, constraints):
    self.__constraints = constraints

  @property
  def system(self):
    return self.__system

  @system.setter
  def system(self, system):
    self.__system = system

  @property
  def property(self):
    return self.__property

  @property.setter
  def property(self, property):
    self.__property = property

  @property
  def material(self):
    return self.__material

  @material.setter
  def material(self, material):
    self.__material = material

