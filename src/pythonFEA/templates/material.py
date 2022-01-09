import logging

import defaults
from .basic import Basic
from .errors import *

class Material(Basic):
  command = 'Material'
  type = 'Material'
  storage_type = 'label'

  def __init__(self, label): # , E, nu, ro, alpha):
    super().__init__(label=label)


