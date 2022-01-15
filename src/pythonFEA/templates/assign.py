import logging

from templates.basic import Collection
from templates.errors import *

class DummyAssignment(Collection):
  command = 'APROP'
  member_types = [int]
  storage_type = 'label'
  assignment_type = 'Property'

  def __init__(self, assignment = None):
    super().__init__()
    if assignment is not None:
      self.assignment = assignment

  def __str__(self):
    out = ''
    for name in self.keys():
      out += f'${type(self).command:s} NAME = \'{name}\'\n'
      eids = ''
      for i, eid in enumerate(self[name]):
        if i % 6 == 0 and i != 0:
          out += '  ' + eids + '\n'
          eids = ''
        eids += f'  {eid:10n}'
      if eids != '':
        out += '  ' + eids + '\n'
      out += '\n'
    return out[:-2]

  @property
  def assignment(self):
    return self.__assignment

  @assignment.setter
  def assignment(self, assignment):
    if type(assignment) in [list, tuple]:
      for [name, eids] in assignment:
        self.add(eids, name)
    elif type(assignment) is dict:
      for name, eids in assignment.items():
        # print(f'{str(name)} - {str(eids)}')
        self.add(name, eids)
    else:
      raise WrongType(f'{self.assignment_type:s} assignment must be one of ({str([t.__name__ for t in self.member_types]):s}).')

  def add(self, name, eids):
    for key in self.keys():
      for eid in eids:
        if eid in self[key]:
          raise UsedIndex(f'Cannot assign more than one {self.assignment_type:s} to Element ID {eid:n} ({key:s}, {name:s}).')
    super().add(members=eids, id=name)

