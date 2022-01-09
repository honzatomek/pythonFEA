import logging

from templates.basic import Basic
from templates.errors import *

class System(Basic):
  command = 'SYSTEM'
  storage_type = 'label'

  def __init__(self, label, amat = None, aprop = None):
    super().__init__(label=label)
    self.amat = amat
    self.aprop = aprop

  def __str__(self):
    out = super().__str__()
    out += '\n'
    out += '  ' + '\n  '.join(str(self.aprop).split('\n')) + '\n'
    out += '\n'
    out += '  ' + '\n  '.join(str(self.amat).split('\n')) + '\n'
    out += f'$END {type(self).command:s}'
    return out

  @property
  def aprop(self):
    return self.__aprop

  @aprop.setter
  def aprop(self, aprop):
    self.__aprop = aprop

  @property
  def amat(self):
    return self.__amat

  @amat.setter
  def amat(self, amat):
    self.__amat = amat

