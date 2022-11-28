#!/usr/bin/python3

import numpy as np
from math import atan2
from math import degrees, radians, sin, cos


class Cartesian:
    def __init__(self, origin, point1, point2, form='xz'):
        self.__o = origin

        if form == 'xz':
            x = (point1 - self.__o) / np.linalg.norm(point1 - self.__o)
            z = (point2 - self.__o) / np.linalg.norm(point2 - self.__o)
            y = np.cross(z, x)
            z = np.cross(x, y)
        elif form == 'zx':
            z = (point1 - self.__o) / np.linalg.norm(point1 - self.__o)
            x = (point2 - self.__o) / np.linalg.norm(point2 - self.__o)
            y = np.cross(z, x)
            z = np.cross(x, y)
        elif form == 'xy':
            x = (point1 - self.__o) / np.linalg.norm(point1 - self.__o)
            y = (point2 - self.__o) / np.linalg.norm(point2 - self.__o)
            z = np.cross(x, y)
            y = np.cross(z, x)
        else:
            raise NotImplementedError(f'Cartesian CSYS formulation {form:s} is '
                                       'not implemented, use xz, zx or xy.')

        self.__T = np.vstack((x, y, z))

    @property
    def O(self):
        '''
        Returns the CSYS origin in global coordinates.
        '''
        return self.__o

    @property
    def T(self):
        '''
        returns the direction sine and cosine matrix transforming global
        coordinates to local ones:

        x_l = T (x_g - x_o),

        where x_o is the cylindrical CSYS origin
        '''
        return self.__T

    def g2l(self, x_g):
        '''
        Transforms a point in global cartesian CSYS to local cartesian CSYS.

        x_l = T (x_g - x_o)

        where x_o is the cylindrical CSYS origin
        '''
        return self.T @ (x_g - self.O)

    def l2g(self, x_l):
        '''
        Transforms a point in global cartesian CSYS to local cartesian CSYS.

        x_g = T^T x_l + x_o

        where x_o is the cylindrical CSYS origin
        '''
        return self.T.T @ x_l - self.O

    def Tr(self, x_g):
        '''
        Constructs a transformation matrix that maps a point in global
        coordinate system to local coordinate system whose z axis coincides
        with current coordinate system and x axis goes through the point x

        x_l = Tr (x_g - x_o)

        returns T
        '''
        x_l = self.g2l(x_g)
        r = (x_l[0] ** 2 + x_l[1] ** 2) ** 0.5

        T = np.array([[x_l[0] / r, -x_l[1] / r, 0],
                      [x_l[1] / r,  x_l[0] / r, 0],
                      [         0,           0, 1]], dtype=float)
        return T @ self.T



class Cylindrical(Cartesian):
    def __init__(self, origin, point1, point2, form='rz'):
        if form == 'rz':
            super().__init__(origin, point1, point2, form='xz')
        elif form == 'zr':
            super().__init__(origin, point1, point2, form='zx')
        elif form == 'rphi':
            super().__init__(origin, point1, point2, form='xy')
        else:
            raise NotImplementedError(f'Cylindrical CSYS formulation {form:s} is '
                                       'not implemented, use rz, zr or rphi.')

    def l2c(self, x_l):
        '''
        Transforms a point in local cartesian CSYS to local cylindrical CSYS.

        r = (x_l ** 2 + y_l ** 2) ** 0.5
        phi = atan2(y_l, x_l)
        z = z_l

        x_c = [r, phi, z]
        '''
        r = (x_l[0] ** 2 + x_l[1] ** 2) ** 0.5
        phi = atan2(x_l[1], x_l[0])
        z = x_l[2]
        return np.array([r, phi, z], dtype=float)

    def g2c(self, x_g):
        '''
        Transforms a point in global cartesian CSYS to local cylindrical CSYS.

        x_l = T (x_g - x_o)

        where x_o is the cylindrical CSYS origin

        r = (x_l ** 2 + y_l ** 2) ** 0.5
        phi = atan2(y_l, x_l)
        z = z_l
        '''
        x = self.g2l(x_g)
        return self.l2c(x)

    def c2l(self, rpz):
        '''
        Transforms a point in local cylindrical coordinates to local cartesian
        coordinates.

        x_l = [ r cos(phi), r sin(phi), z)
        '''
        return np.array([rpz[0] * cos(rpz[1]), rpz[0] * sin(rpz[1]), rpz[2]], dtype=float)

    def c2g(self, rpz):
        '''
        Transforms a point in local cylindrical coordinates to global cartesian
        coordinates.

        x_l = [ r cos(phi), r sin(phi), z)

        x_g = T^T x_l + x_o
        '''
        return self.T.T @ self.c2l(rpz) + self.O



if __name__ == '__main__':
    r = 15.
    phi_step = radians(15.)

    phi = np.linspace(0, radians(360) - phi_step, int(radians(360) / phi_step))
    print(f'{np.degrees(phi) = }')

    cyl = Cylindrical(np.array([0., 0., 0.]),
                      np.array([1., 0., 0.]),
                      np.array([0., 0., 1.]), form='rz')

    for i in range(phi.shape[0]):
        xyz = cyl.c2l(np.array([r, phi[i], 0.], dtype=float))
        rr = (xyz[0] ** 2 + xyz[1] ** 2) ** 0.5
        print(f'{i = }, {xyz = }, {rr = }')


