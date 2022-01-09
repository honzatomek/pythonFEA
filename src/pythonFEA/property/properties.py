import logging

import defaults
from templates.errors import *
from templates.basic import Collection
from templates.property import Property

from .property import PBeam2D

class Properties(Collection):
  command = 'PROPERTY'
  type = 'Properies'
  member_types = [PBeam2D]
  storage_type = 'label'

  def __str__(self):
    out = f'$ENTER {type(self).command:s}\n'
    for name, prop in self.items():
      out += '  ' + '\n  '.join(str(prop).split('\n'))
      out += '\n'
    out += f'$EXIT {type(self).command:s}\n'
    return out


