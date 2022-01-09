import logging
import math
import numpy as np

import defaults
from .elements import *
from templates.basic import Collection
from templates.errors import *
from templates.element import Element
from .element.beam2d import Beam2D

class Elements(Collection):
  command = 'ELEMENT'
  type = 'Elements'
  member_types = [Element]
  storage_type = 'id'

