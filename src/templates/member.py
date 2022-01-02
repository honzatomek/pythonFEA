from .errors.errors import *

class Member:
    TYPE = 'Member'

    def __check_id(self, id):
        if id is None:
            raise MissingEntry(f'{str(TYPE)} ID missing')
        if type(id) is not int:
            raise NotValidID(f'{str(TYPE)} ID is not an int ({str(id)}, {str(type(id))}),')
        if id < 1:
            raise NotValidID(f'{str(TYPE)} ID ({str(id)}) < 1.'}

    def __init__(self, id: int, label: str = None):
        self.__check_id(id)
        self.__id = id

        if label is not None:
            self.__label = label
        else:
            self.__label = None

    @property
    def id(self):
        return self.__id

    @id.setter
    def id(self, id: int):
        self.__check_id(id)
        self.__id = id

    @property
    def label(self):
        return self.__label

    @label.setter
    def label(self, label: str):
        if label is not None:
            self.__label = str(label)
        else:
            self.__label = None


class Members:
    TYPE = 'Member'
    MEMBER_TYPE = Member

    def __is_in(self, id: int):
        if id is None:
            raise MissingEntry(f'{str(TYPE)} ID missing')
        if type(id) is not int:
            raise NotValidID(f'{str(TYPE)} ID is not an int ({str(id)}, {str(type(id))}),')
        if id not in self.__dict.keys():
            raise NotValidID(f'{str(TYPE)} ID {(str(id)} already exists.')
        return True

    def __check_id(self, id):
        if id is None:
            raise MissingEntry(f'{str(TYPE)} ID missing')
        if id in self.__node.keys():
            raise NotValidID(f'{str(TYPE)} ID {(str(id)} already exists.')

    def __check(self, member):
        if member is None:
            raise MissingEntry(f'{str(TYPE)} information missing ({str(member)}).')
        if type(member) is not MEMBER_TYPE:
            raise TypeError(f'{str(TYPE)} entry is not of type {str(MEMBER_TYPE)} ({str(type(member))}).')
        self.__check_id(member.id)

    def __init__(self):
        self.__count = 0
        self.__dict = dict()

    def __getitem__(self, id):
        return self.__dict[id]

    def __iter__(self):
        return iter(self.__dict)

    def __len__(self):
        return self.__count

    def add(self, **kwargs):
        if 'member' in kwargs.keys()::
            member = kwargs['member']
            self.__check(member)
            self.__dict[member] = deepcopy(member)
            self.__count += 1
        elif 'id' in kwargs.keys():
                id = kwargs['id']
            else:
                raise AttributeError(f'ID is missing.')
            self.__check_id(id)
            self.__dict[id] = MEMBER_TYPE(**kwargs)
            self.__count += 1
        else:
            raise AttributeError(f'{str(TYPE)} ID or entries missing.')

    def keys(self):
        return self.__dict.keys()

    def values(self):
        return self.__dict.values()

    def items(self):
        return self.__dict.items()

    @property
    def count(self):
        return self.__count


