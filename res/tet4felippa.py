#!/usr/bin/python3

import numpy as np

VERBOSE = True
np.set_printoptions(precision=4, suppress=True)


def IsoTetr4Strain(disp):
    Bx, By, Bz, Nx, Ny, Nz, Jdet = IsoTetr4ShapeFunCarDer(coor)

    Be = np.array([np.array([[Bx[i], 0., 0.] for i in range(coor.shape[0])], dtype=float).flatten(),
                   np.array([[0., By[i], 0.] for i in range(coor.shape[0])], dtype=float).flatten(),
                   np.array([[0., 0., Bz[i]] for i in range(coor.shape[0])], dtype=float).flatten(),
                   np.array([[By[i], Bx[i], 0.] for i in range(coor.shape[0])], dtype=float).flatten(),
                   np.array([[0., Bz[i], By[i]] for i in range(coor.shape[0])], dtype=float).flatten(),
                   np.array([[Bz[i], 0., Bx[i]] for i in range(coor.shape[0])], dtype=float).flatten()], dtype=float)

    ue = disp.flatten().T
    if VERBOSE:
        print(f'ue =\n{ue}')

    ee = Be @ ue
    if VERBOSE:
        print(f'ee =\n{ee}')

    return ee


def IsoTetr4Stress(disp, E, nu):
    Emat = emat3D(E, nu)

    ee = IsoTetr4Strain(disp)

    sig = Emat @ ee.T
    if VERBOSE:
        print(f'sig =\n{sig}')

    return sig


def HuberMisesHencky(stress):
    vm = np.sqrt(stress[:3] @ stress[:3].T + 3 * (stress[3:] @ stress[3:].T))
    if VERBOSE:
        print(f'vm = {vm:.4f}')
    return vm


def PrincipalStress(sig):
    S = np.array([[sig[0], sig[3], sig[5]],
                  [sig[3], sig[1], sig[4]],
                  [sig[5], sig[4], sig[2]]], dtype=float)

    e_val, e_vec = np.linalg.eigh(S)

    # principal normal stress
    p3, p2, p1 = np.sort(e_val)
    e_val_l = e_val.tolist()
    p1i, p2i, p3i = e_val_l.index(p1), e_val_l.index(p2), e_val_l.index(p3)

    # principal plane
    p1v, p2v, p3v = e_vec[:,p1i], e_vec[:,p2i], e_vec[:,p3i]

    # principal shear stress
    t1 = (p1 - p3) / 2
    t2 = (p1 - p2) / 2
    t3 = (p2 - p3) / 2

    # principal shear plane
    t1v = (p1v + p3v) / np.linalg.norm(p1v + p3v)
    t2v = (p1v + p2v) / np.linalg.norm(p1v + p2v)
    t3v = (p2v + p3v) / np.linalg.norm(p2v + p3v)

    return p1, p2, p3


def IsoTetr4Stiffness(coor, E, nu, rho, F):
    Bx, By, Bz, Nx, Ny, Nz, Jdet = IsoTetr4ShapeFunCarDer(coor)

    ndof = 3

    Ne = np.array([np.array([[Nx[i], 0., 0.] for i in range(coor.shape[0])], dtype=float).flatten(),
                   np.array([[0., Ny[i], 0.] for i in range(coor.shape[0])], dtype=float).flatten(),
                   np.array([[0., 0., Nz[i]] for i in range(coor.shape[0])], dtype=float).flatten()], dtype=float)
    if VERBOSE:
        print(f'Ne =\n{Ne}')

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

    disp = np.array([[0., 0., 0.],
                     [0., 0., 0.],
                     [0., 0., 0.],
                     [.1, .1, .1]], dtype=float)

    strain = IsoTetr4Strain(disp)
    stress = IsoTetr4Stress(disp, E, nu)
    vm = HuberMisesHencky(stress)
    s1, s2, s3 = PrinicpalStress(stress)

