from ...errors.errors import *
from ...templates.member import Member, Members
from copy import deepcopy

class Node(Member):
    TYPE = 'Node'

    def __init__(self, id: int, x: float, y: float, z: float, label: str = None, csys: int = 0):
        super().__init__(id, label)

        self.__check_coor(x, 'X')
        self.__check_coor(y, 'Y')
        self.__check_coor(z, 'Z')
        self.__x = x
        self.__y = y
        self.__z = z

        self.__csys = csys

    def __check_coor(self, coor, dir):
        if coor is None:
            raise MissingEntry(f'Node {str(self.id)} {dir}-Coordinate is missing.')
        if type(coor) is not float:
            raise NotValidCoor(f'Node {str(self.id)} {dir}-Coordinate is not a float ({str(coor)}, {str(type(coor))}).')

    @property
    def x(self):
        return self.__x

    @property
    def y(seelf):
        return self.__y

    @property
    def z(self):
        return self.__z

    @x.setter
    def x(self, x):
        self.__check_coor(x, 'X')
        self.__x = x

    @y.setter
    def y(self, y):
        self.__check_coor(y, 'Y')
        self.__y = y

    @z.setter
    def z(self, z):
        self.__check_coor(z, 'Z')
        self.__z = z

    @property
    def xyz(self):
        return [self.x, self.y, self.z]

    @xyz.setter
    def xyz(self, xyz: list):
        if type(xyz) is not list:
            raise NotValidCoor(f'XYZ Values of Node ID {str(self.id)} must be a list of 3 floats ({str(xyz)}).')
        if len(xyz) != 3:
            raise NotValidCoor(f'XYZ Values of Node ID {str(self.id)} must be a list of 3 floats ({str(xyz)}).')
        self.x = xyz[0]
        self.y = xyz[1]
        self.z = xyz[2]

    @property
    def csys(self):
        return self.__csys

    @csys.setter
    def csys(self, csys_id: int):
        self.__csys = csys_id


class Nodes:
    TYPE = 'Node'
    member_type = Node

    def __init__(self):
        self.__count = 0
        self.__node = dict()

    def distance(self, nd1: int, nd2: int):
        if self.__is_in(nd1) and self.__is_in(nd2):
            n1 = self[nd1].xyz
            n2 = self[nd2].xyz

            d = 0
            for i in range(3):
                d = (n2[i] - n1[i]) ^ 2
            return d ^ (0.5)







