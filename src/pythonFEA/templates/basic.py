import logging

import defaults
from .errors import *

class Basic:
  command = 'BASIC'
  # type = 'Basic'
  storage_type = 'id'

  def __init__(self, id = None, label = None):
    if id is None and label is None:
        raise IDLabelError(f'Either an id or a label must be specified')
    self.id = id
    self.label = label

    logging.debug(f'Created {repr(self):s}.')

  def __str__(self):
    if type(self).storage_type == 'id':
      if self.label is not None:
        return f'{self.id:8n} : \'{self.label:s}\''
      else:
        return f'{self.id:8n}'
    elif type(self).storage_type == 'label':
      return f'${type(self).command:s} NAME = {self.label:s}'

  def __repr__(self):
    if type(self).storage_type == 'id':
      return f'{type(self).__name__:s} ID {self.id:n}'
    elif type(self).storage_type == 'label':
      return f'{type(self).__name__:s} NAME {self.label:s}'

  @property
  def id(self):
    return self.__id

  @id.setter
  def id(self, id):
    if id is None:
      if type(self).storage_type == 'id':
        raise NotValidID(f'{type(self).__name__:s} ID is not an \'int\' ({type(id).__name__:s}).')
      self.__id = None
      return
    if type(id) is not int:
      raise NotValidID(f'{type(self).__name__:s} ID is not an \'int\' ({type(id).__name__:s}).')
    if id < 1:
      raise NotValidID(f'{type(self).__name__:s} ID must be > 0 (not {id:n}).')
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
        raise NotValidLable(f'{type(self).__name__:s} Label must be defined.')
      self.__label = None


class Collection:
  command = 'COLLECTION'
  # type = 'Collection'
  member_types = [Basic]
  storage_type = 'id'

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
    if self.storage_type == 'id':
      return f'{type(self).__name__:s}: count = {self.count:n}, min ID = {self.min:n}, max ID = {self.max:n}'
    elif self.storage_type == 'label':
      return f'{type(self).__name__:s}: count = {self.count:n}'

  @property
  def count(self):
    return self.__count

  @property
  def min(self):
    if self.storage_type == 'id':
      if len(self.__members.keys()) > 0:
        return min(self.__members.keys())
      else:
        return 0
    elif self.storage_type == 'label':
      if len(self.__members.keys()) > 0:
        return self.__members.keys()[0]
      else:
        return None
    return None

  @property
  def max(self):
    if self.storage_type == 'id':
      if len(self.__members.keys()) > 0:
        return max(self.__members.keys())
      else:
        return 0
    elif self.storage_type == 'label':
      if len(self.__members.keys()) > 0:
        return self.__members.keys()[-1]
      else:
        return None
    return None

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
    if self.storage_type == 'id':
      if type(id) is not int:
        raise WrongType(f'{type(self).__name__:s} ID is not an \'int\' ({type(id).__name__:s}).')
      if id not in self.__members.keys():
        raise MissingIndex(f'{type(self).__name__:s} is missing entry ID {id:n}.')
    elif self.storage_type == 'label':
      if type(id) is not str:
        raise WrongType(f'{type(self).__name__:s} ID is not an \'str\' ({type(id).__name__:s}).')
      if id not in self.__members.keys():
        raise MissingLabel(f'{type(self).__name__:s} is missing entry ID {id:s}.')
    return self.__members[id]

  def __add(self, member, id = None):
    # if type(member) not in (type(self).member_types):
    if not any([isinstance(member, t) for t in type(self).member_types]):
      raise WrongType(f'{type(self).__name__:s} cannot add {type(member).__name__:s}, type conflict (not {", ".join([t.__name__ for t in self.member_types]):s})')
    if type(self).storage_type == 'id':
      if type(member.id) is not int:
        raise WrongType(f'{type(self).__name__:s} ID is not an \'int\' ({type(self).__name__:s}).')
      if member.id in self.__members.keys():
        raise UsedIndex(f'{type(self).__name__:s} already contains entry ID {member.id:n}.')
      self.__members[member.id] = member
    elif type(self).storage_type == 'label':
      if id is not None:
        self.__members.setdefault(id, [])
        self.__members[id].append(member)
        # print(f'{id} - {str(member)}')
      else:
        if member.label in self.__members.keys():
          raise UsedLabel(f'{type(self).__name__:s} already contains entry ID {member.label:s}.')
        self.__members[member.label] = member
    else:
      raise NotYetImplemented(f'storage_type must be either an id or label.')
    self.__count += 1

    logging.debug(f'Added \'{repr(member):s}\' to {type(self).__name__:s}.')

  def add(self, members, id = None):
    if type(members) in (list, tuple):
      # print('in list')
      if id is not None:
        # print('id is not None')
        for m in members:
          self.__add(m, id)
      else:
        for m in members:
          self.__add(m)
    else:
      if id is not None:
        self.__add(members, id)
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

