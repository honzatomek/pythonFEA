import logging

import defaults
from templates.errors import *
from templates.basic import Collection
from .material import LinearElastic

class Materials(Collection):
  command = 'MATERIAL'
  type = 'Materials'
  member_types = [LinearElastic]
  storage_type = 'label'

  def __str__(self):
    out = f'$ENTER {type(self).command:s}\n'
    for name, mat in self.items():
      out += '  ' + '\n  '.join(str(mat).split('\n'))
      out += '\n'
    out += f'$EXIT {type(self).command:s}\n'
    return out

