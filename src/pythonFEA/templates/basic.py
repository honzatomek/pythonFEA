import defaults
from templates.errors import *
import logging

class Basic:
  command = 'BASIC'
  type = 'Basic'
  storage_type = 'id'

  def __init__(self, id = None, label = None):
    if id is None and label is None:
        raise IDLabelError(f'Either an id or a label must be specified')
    self.id = id
    self.label = label

    logging.debug(f'Created {repr(self):s}.')

  def __str__(self):
    if self.label is not None:
      return f'{self.id:8n} : \'{self.label:s}\''
    else:
      return f'{self.id:8n}'

  def __repr__(self):
    return f'{type(self).type:s} ID {self.id:n}'

  @property
  def id(self):
    return self.__id

  @id.setter
  def id(self, id):
    if id is None:
      if self.storage_type == 'id':
        raise NotValidID(f'{type(self).type:s} ID is not an \'int\' ({type(id).__name__:s}).')
      self.__id = None
    if type(id) is not int:
      raise NotValidID(f'{type(self).type:s} ID is not an \'int\' ({type(id).__name__:s}).')
    if id < 1:
      raise NotValidID(f'{type(self).type:s} ID must be > 0 (not {id:n}).')
    self.__id = id

  @property
  def label(self):
    return self.__label

  @label.setter
  def label(self, label = None):
    if label is not None:
      self.__label = str(label)[0:defaults.DEFAULT_LABEL_LENGTH]
    else:
      if self.storage_type == 'label':
        raise NotValidLable(f'{type(self).type:s} Label must be defined.')
      self.__label = None


class Collection:
  command = 'COLLECTION'
  type = 'Collection'
  member_types = [Basic]

  def __init__(self, label = None):
    self.__count = 0
    self.__members = dict()

    self.label = label

    logging.debug(f'Created {repr(self):s}.')

  def __str__(self):
    string = ''
    command = ''
    for i, m in self.__members.items():
      if type(m).command != command:
        string += '\n' if command != '' else ''
        if self.label is not None:
          string += f'  ${type(m).command} SET = \'{self.label}\'\n'
        else:
          string += f'  ${type(m).command}\n'
        command = type(m).command
      string += '    ' + str(m) + '\n'
    return string

  def __repr__(self):
    return f'{type(self).type:s}: count = {self.count:n}, min ID = {self.min:n}, max ID = {self.max:n}'

  @property
  def count(self):
    return self.__count

  @property
  def min(self):
    if len(self.__members.keys()) > 0:
      return min(self.__members.keys())
    else:
      return 0

  @property
  def max(self):
    if len(self.__members.keys()) > 0:
      return max(self.__members.keys())
    else:
      return 0

  @property
  def label(self):
    return self.__label

  @label.setter
  def label(self, label = None):
    if label:
      self.__label = str(label)[0:defaults.DEFAULT_LABEL_LENGTH]
    else:
      self.__label = None

  def id(self, id):
    if type(id) is not int:
      raise WrongType(f'{type(self).type:s} ID is not an \'int\' ({type(id).__name__:s}).')
    if id not in self.__members.keys():
      raise MissingIndex(f'{type(self).type:s} is missing entry ID {id:n}.')
    return self.__members[id]

  def __add(self, member):
    if type(member) not in (self.member_types):
      raise WrongType(f'{type(self).type:s} cannot add {type(member).__name__:s}, type conflict (not {", ".join([t.__name__ for t in self.member_types]):s})')
    if type(member.id) is not int:
      raise WrongType(f'{type(self).type:s} ID is not an \'int\' ({type(self).__name__:s}).')
    if member.id in self.__members.keys():
      raise UsedIndex(f'{type(self).type:s} already contains entry ID {member.id:n}.')
    self.__members[member.id] = member
    self.__count += 1

    logging.debug(f'Added \'{repr(member):s}\' to {type(self).type:s}.')

  def add(self, members):
    if type(members) in (list, tuple):
      for m in members:
        self.__add(m)
    else:
      self.__add(members)

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

