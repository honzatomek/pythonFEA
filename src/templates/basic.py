from errors.errors import *
import math

class Basic:
  command = 'BASIC'
  type = 'Basic'

  def __init__(self, id, label = None):
    self.id = id
    self.label = label

  def __str__(self):
    if self.label:
      return f'{self.id:8n} : \'{self.label:s}\''
    else:
      return f'{self.id:8n}'

  def __repr__(self):
    return f'{self.type:s} ID {self.id:n}'

  @property
  def id(self):
    return self.__id

  @id.setter
  def id(self, id):
    if type(id) is not int:
      raise WrongType(f'{self.type:s} ID is not an \'int\' ({type(self).__name__:s}).')
    self.__id = id

  @property
  def label(self):
    return self.__label

  @label.setter
  def label(self, label = None):
    if label:
      self.__label = str(label)
    else:
      self.__label = None


class Collection:
  command = 'COLLECTION'
  type = 'Collection'
  member_types = [Basic]

  def __init__(self):
    self.__count = 0
    self.__members = dict()

    self.__initialised = False

  def __str__(self):
    string = ''
    command = ''
    for i, m in self.__members.items():
      if m.command != command:
        string += '\n' if command != '' else ''
        if m.label:
          string += f'  ${m.command:s}\n'
        else:
          string += f'  ${m.command:s} NAME = \'{m.label:s}\'\n'
        command = m.command
      string += '    ' + str(m) + '\n'
    return string

  def __repr__(self):
    return f'{self.type:s}: count = {self.count:n}, min ID = {self.min:n}, max ID = {self.max:n}'

  @property
  def count(self):
    return self.__count

  @property
  def min(self):
    if len(self.__members.keys()) > 0:
      return math.min(self.__members.keys())
    else:
      return 0

  @property
  def max(self):
    if len(self.__members.keys()) > 0:
      return math.max(self.__members.keys())
    else:
      return 0

  def id(self, id):
    if type(id) is not int:
      raise WrongType(f'{self.type:s} ID is not an \'int\' ({type(self).__name__:s}).')
    if id not in self.__members.keys():
      raise MissingIndex(f'{self.type:s} is missing entry ID {id:n}.')
    return self.__members[id]

  def add(self, member):
    if type(member) not in (self.member_types):
      raise WrongType(f'{self.type:s} cannot add {member.type:s}, type conflict (not {", ".join([t.__name__ for t in self.member_types]):s})')
    if type(member.id) is not int:
      raise WrongType(f'{self.type:s} ID is not an \'int\' ({type(self).__name__:s}).')
    if member.id in self.__members.keys():
      raise UsedIndex(f'{self.type:s} already contains entry ID {member.id:n}.')
    self.__members[member.id] = member

  def __getitem__(self, id):
    return self.id(id)

  def __iter__(self):
    return iter(self.__members)

  def keys(self):
    return self.__members.keys()

  def values(self):
    return self.__members.values()

  def items(self):
    return self.__members.items()


a = Collection()
a.add(Basic(1, 'test 1'))
a.add(Basic(2, 'test 2'))
a.add(Basic(3, 'test 3'))
a.add(Basic(4, 'test 4'))

for i, b in a.items():
  print(f'{i:n} :: {str(b):s}')
