#!/usr/bin/python3

import numpy as np

VERBOSE = True
np.set_printoptions(precision=4, suppress=True)

def IsoTetr4Stiffness(coor, E, nu, rho, F):
    Bx, By, Bz, Nx, Ny, Nz, Jdet = IsoTetr4ShapeFunCarDer(coor)

    ndof = 3

    Ne = np.array([np.array([[Nx[i], 0., 0.] for i in range(coor.shape[0])], dtype=float).flatten(),
                   np.array([[0., Ny[i], 0.] for i in range(coor.shape[0])], dtype=float).flatten(),
                   np.array([[0., 0., Nz[i]] for i in range(coor.shape[0])], dtype=float).flatten()], dtype=float)
    if VERBOSE:
        print(f'Ne =\n{Ne}')

    o = np.zeros(coor.shape[0], dtype=float)
    # Be = np.array([[*Bx,  *o,  *o],
    #                [ *o, *By,  *o],
    #                [ *o,  *o, *Bz],
    #                [*By, *Bx,  *o],
    #                [ *o, *Bz, *By],
    #                [*Bz,  *o, *Bx]], dtype=float)
    Be = np.array([np.array([[Bx[i], 0., 0.] for i in range(coor.shape[0])], dtype=float).flatten(),
                   np.array([[0., By[i], 0.] for i in range(coor.shape[0])], dtype=float).flatten(),
                   np.array([[0., 0., Bz[i]] for i in range(coor.shape[0])], dtype=float).flatten(),
                   np.array([[By[i], Bx[i], 0.] for i in range(coor.shape[0])], dtype=float).flatten(),
                   np.array([[0., Bz[i], By[i]] for i in range(coor.shape[0])], dtype=float).flatten(),
                   np.array([[Bz[i], 0., Bx[i]] for i in range(coor.shape[0])], dtype=float).flatten()], dtype=float)
    if VERBOSE:
        print(f'Be =\n{Be}')

    Emat = emat3D(E, nu)

    Ke = Jdet / 6. * Be.T @ Emat @ Be
    # 1x 1y 1z 2x 2y 2z
    headers = np.array([[f'{i+1:n}{x:s}' for x in ['x', 'y', 'z']] for i in range(coor.shape[0])]).flatten()
    if VERBOSE:
        print('Ke =')
        msg = '{0:^11s}|'.format('00') + ''.join(['{0:^12s}'.format(h) for h in headers])
        print(msg)
        print('-' * len(msg))
        for i in range(Ke.shape[0]):
            print(f'{headers[i]:^11s}|' + ''.join([f'{Ke[i,j]:12.2f}' for j in range(Ke.shape[1])]))
        print(f'eig(Ke) =\n{np.linalg.eigh(Ke)[0]}')

    Me = Jdet / 6. * rho * (Ne.T @ Ne)
    if VERBOSE:
        print(f'Me =\n{Me}')

    Fe = Jdet / 6. * (Ne.T @ F)
    if VERBOSE:
        print(f'Fe =\n{Fe}')

    return Ke, Me, Fe


def chop(A, eps=1E-10):
    B = np.copy(A)
    B[np.abs(A) < eps] = 0.
    return B


def IsoTetr4ShapeFunCarDer(coor):
    if VERBOSE:
        print(f'coor =\n{coor}')

    xn, yn, zn = coor[:,0], coor[:,1], coor[:,2]
    if VERBOSE:
        print(f'xn =\n{xn}')
        print(f'yn =\n{yn}')
        print(f'zn =\n{zn}')

    dNz = np.eye(4, dtype=float)
    if VERBOSE:
        print(f'dNz =\n{dNz}')

    # Jmat = dNz @ np.vstack((np.ones((1, coor.shape[0]), dtype=float), coor.T))
    Jmat = np.ones((4, 4), dtype=float)
    for i, X in enumerate([xn, yn, zn]):
        for j in range(dNz.shape[0]):
            Jmat[i+1,j] = dNz[j] @ X
    if VERBOSE:
        print(f'Jmat =\n{Jmat}')

    # Jdet = np.linalg.det(Jmat)
    Jdet = ((Jmat[1,2]*Jmat[2,1]-Jmat[1,1]*Jmat[2,2]+Jmat[1,3]*Jmat[2,2]-Jmat[1,3]*Jmat[2,1]+Jmat[1,1]*Jmat[2,3]-Jmat[1,2]*Jmat[2,3])*Jmat[3,0]-
            (Jmat[1,2]*Jmat[2,0]-Jmat[1,0]*Jmat[2,2]+Jmat[1,3]*Jmat[2,2]-Jmat[1,3]*Jmat[2,0]+Jmat[1,0]*Jmat[2,3]-Jmat[1,2]*Jmat[2,3])*Jmat[3,1]+
            (Jmat[1,1]*Jmat[2,0]-Jmat[1,0]*Jmat[2,1]+Jmat[1,3]*Jmat[2,1]-Jmat[1,3]*Jmat[2,0]+Jmat[1,0]*Jmat[2,3]-Jmat[1,1]*Jmat[2,3])*Jmat[3,2]-
            (Jmat[1,1]*Jmat[2,0]-Jmat[1,0]*Jmat[2,1]+Jmat[1,2]*Jmat[2,1]-Jmat[1,2]*Jmat[2,0]+Jmat[1,0]*Jmat[2,2]-Jmat[1,1]*Jmat[2,2])*Jmat[3,3])
    if VERBOSE:
        print(f'Jdet = {Jdet} vs. {np.linalg.det(Jmat)}')

    # Jinv = np.linalg.inv(Jmat) * Jdet
    Jinv = np.array([[Jmat[2,1]*(Jmat[3,3]-Jmat[3,2])-Jmat[2,2]*(Jmat[3,3]-Jmat[3,1])+Jmat[2,3]*(Jmat[3,2]-Jmat[3,1]),
                     -Jmat[1,1]*(Jmat[3,3]-Jmat[3,2])+Jmat[1,2]*(Jmat[3,3]-Jmat[3,1])-Jmat[1,3]*(Jmat[3,2]-Jmat[3,1]),
                      Jmat[1,1]*(Jmat[2,3]-Jmat[2,2])-Jmat[1,2]*(Jmat[2,3]-Jmat[2,1])+Jmat[1,3]*(Jmat[2,2]-Jmat[2,1])],
                    [-Jmat[2,0]*(Jmat[3,3]-Jmat[3,2])+Jmat[2,2]*(Jmat[3,3]-Jmat[3,0])-Jmat[2,3]*(Jmat[3,2]-Jmat[3,0]),
                      Jmat[1,0]*(Jmat[3,3]-Jmat[3,2])-Jmat[1,2]*(Jmat[3,3]-Jmat[3,0])+Jmat[1,3]*(Jmat[3,2]-Jmat[3,0]),
                     -Jmat[1,0]*(Jmat[2,3]-Jmat[2,2])+Jmat[1,2]*(Jmat[2,3]-Jmat[2,0])-Jmat[1,3]*(Jmat[2,2]-Jmat[2,0])],
                     [Jmat[2,0]*(Jmat[3,3]-Jmat[3,1])-Jmat[2,1]*(Jmat[3,3]-Jmat[3,0])+Jmat[2,3]*(Jmat[3,1]-Jmat[3,0]),
                     -Jmat[1,0]*(Jmat[3,3]-Jmat[3,1])+Jmat[1,1]*(Jmat[3,3]-Jmat[3,0])-Jmat[1,3]*(Jmat[3,1]-Jmat[3,0]),
                      Jmat[1,0]*(Jmat[2,3]-Jmat[2,1])-Jmat[1,1]*(Jmat[2,3]-Jmat[2,0])+Jmat[1,3]*(Jmat[2,1]-Jmat[2,0])],
                    [-Jmat[2,0]*(Jmat[3,2]-Jmat[3,1])+Jmat[2,1]*(Jmat[3,2]-Jmat[3,0])-Jmat[2,2]*(Jmat[3,1]-Jmat[3,0]),
                      Jmat[1,0]*(Jmat[3,2]-Jmat[3,1])-Jmat[1,1]*(Jmat[3,2]-Jmat[3,0])+Jmat[1,2]*(Jmat[3,1]-Jmat[3,0]),
                     -Jmat[1,0]*(Jmat[2,2]-Jmat[2,1])+Jmat[1,1]*(Jmat[2,2]-Jmat[2,0])-Jmat[1,2]*(Jmat[2,1]-Jmat[2,0])]], dtype=float)
    if VERBOSE:
        print(f'Jinv =\n{Jinv}')


    B = Jinv.T @ dNz / Jdet
    if VERBOSE:
        print(f'B =\n{B}')

    return B[-3], B[-2], B[-1], Jmat[-3], Jmat[-2], Jmat[-1], Jdet

def emat3D(E, nu):
    Emat = E / ((1.0 + nu) * (1.0 - 2.0 * nu)) * np.array([[1.0 - nu, nu, nu, 0.0, 0.0, 0.0],
                                                          [nu, 1.0 - nu, nu, 0.0, 0.0, 0.0],
                                                          [nu, nu, 1.0 - nu, 0.0, 0.0, 0.0],
                                                          [0.0, 0.0, 0.0, (1.0 - 2.0 * nu) / 2.0, 0.0, 0.0],
                                                          [0.0, 0.0, 0.0, 0.0, (1.0 - 2.0 * nu) / 2.0, 0.0],
                                                          [0.0, 0.0, 0.0, 0.0, 0.0, (1.0 - 2.0 * nu) / 2.0]], dtype=float)
    if VERBOSE:
        print(f'Emat =\n{Emat}')

    return Emat


if __name__ == '__main__':
    coor = np.array([[2., 3., 4.],
                     [6., 3., 2.],
                     [2., 5., 1.],
                     [4., 3., 6.]], dtype=float)

    E, nu, rho = 96., 1./3., 7.8
    fx, fy, fz = 0., 0., 0.
    Ke = IsoTetr4Stiffness(coor, E, nu, rho, np.array([fx, fy, fz]))

