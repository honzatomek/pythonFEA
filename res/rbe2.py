#!/usr/bin/python

import numpy as np

def ROD_Kl(n1, n2, E, A):
    '''
    ROD local stiffness matrix
    In:
        n1 - Node 1 [x1, z1]
        n2 - Node 2 [x2, z2]
        E  - Young's Modulus
        A  - Cross Section Area
    Out:
        Kl - ROD stiffness matrix in LCS
    '''
    L = ((n2[0] - n1[0]) ** 2 + (n2[1] - n1[1]) ** 2) ** 0.5
    K_l = E * A / L * np.array([[ 1, 0, -1, 0],
                                [ 0, 0,  0, 0],
                                [-1, 0,  1, 0],
                                [ 0, 0,  0, 0]], dtype=float)

    return K_l


def ROD_T(n1, n2):
    '''
    ROD transformation matrix
    In:
        n1 - Node 1 [x1, z1]
        n2 - Node 2 [x2, z2]
    Out:
        T  - ROD transformation matrix LCS -> GCS
    '''
    L = ((n2[0] - n1[0]) ** 2 + (n2[1] - n1[1]) ** 2) ** 0.5
    c = (n2[0] - n1[0]) / L
    s = (n2[1] - n1[1]) / L
    T = np.array([[ c, s,  0, 0],
                  [-s, c,  0, 0],
                  [ 0, 0,  c, s],
                  [ 0, 0, -s, c]], dtype=float)

    return T


def ROD_Kg(n1, n2, E, A):
    '''
    ROD global stiffness matrix
    In:
        n1 - Node 1 [x1, z1]
        n2 - Node 2 [x2, z2]
        E  - Young's Modulus
        A  - Cross Section Area
    Out:
        Kg - ROD stiffness matrix in GCS
    '''
    Kl = ROD_Kl(n1, n2, E, A)
    T = ROD_T(n1, n2)
    Kg = T.T @ Kl @ T

    return Kg


def assemble_K(K, Ke, lme):
    '''
    Assemble master stiffness matrix
    In:
        K   - master stiffness matrix [m, m]
        Ke  - element stiffness matrix in GCS [n, n]
        lme - element localisation matrix [global dofs]
    Out:
        K   - master stiffness matrix with new element added [m ,m]
    '''

    for r, dr in enumerate(lme): # row, DOF row
        for c, dc in enumerate(lme): # column, DOF column
            K[dr, dc] += Ke[r, c]

    return K


def RBE2(nodes, connectivity):
    '''
    RBE2 definition

    Transformation Matrix:

        [ u_s ]   [1  0   z]   [ u_m ]
        [ w_s ] = [0  1  -x] x [ w_m ]
        [phi_s]   [0  0   1]   [phi_m]

        where:
            x = x_s - x_m
            z = z_s - z_m

    In:
        nodes        - array of node coordinates [x, z]
        connectivity - array of rbe2 connectivity
                       [[master nID, dofX, dofZ, dofPhi],
                        [slave  nID, dofX, dofZ, dofPhi],
                                     ...
                        [slave  nID, dofX, dofZ, dofPhi]]
                        where dof? == 1 if DOF is connected, 0 otherwise

    Out:
        T            - master-slave transformation matrix u_s = T u_m
        sDOFs        - slave DOF rows [sID, DOF] for each row of T
    '''
    mID = connectivity[0,0]
    mDOF = connectivity[0,1:4]
    mXZ = nodes[mID]
    mDOFs = [i for i in range(mDOF.shape[0]) if mDOF[i] == 1]
    # print(f'{mDOFs = }')

    T = []
    sDOFs = []
    for i in range(1, connectivity.shape[0]):
        sID = connectivity[i,0]
        sDOF = connectivity[i,1:4]
        sXZ = nodes[sID]
        x = sXZ[0] - mXZ[0]
        z = sXZ[1] - mXZ[1]

        for j in range(sDOF.shape[0]):
            d = sDOF[j]
            if d == 1:
                sDOFs.append([sID, j])
                if j == 0:
                    T.append([1., 0.,  z ])
                elif j == 1:
                    T.append([0., 1., -x ])
                elif j == 2:
                    T.append([0., 0.,  1.])

    T = np.array(T, dtype=float)
    sDOFs = np.array(sDOFs, dtype=int)

    return T[:,mDOFs], sDOFs


def set_dofs(nodes, connectivity, rbe, spc):
    '''
    Assign DOFs to Elements

    First unconstrained DOFs are assigned, then RBE master DOFs, RBE slave
    DOFs, lastly SPC DOFs.

    In:
        nodes        - list of node coordinates
        connectivity - element connectivity [nID1, nID2]
        rbe          - rbe connectivity [master, slave 1, ..., slave n]
        spc          - SPC dofs [nID, dofX, dofZ] - if dof == 1 then constrained
    '''


if __name__ == '__main__':
    # nodes
    xz = np.array([[   0.,    0.],               # 0 - SPC 1 2
                   [   0., 1000.],               # 1 - SPC   2
                   [1000.,    0.],               # 2
                   [1000., 1000.],               # 3
                   [2000.,    0.],               # 4 - RBE2 Slave 1
                   [2000., 1000.],               # 5 - RBE2 Slave 2
                   [2500.,  500.]], dtype=float) # 6 - RBE2 Master

    # elements
    connectivity = np.array([[0, 1],             # 0 [start, end]
                             [0, 2],             # 1 [start, end]
                             [1, 3],             # 2 [start, end]
                             [0, 3],             # 3 [start, end]
                             [2, 3],             # 4 [start, end]
                             [2, 4],             # 5 [start, end]
                             [3, 5],             # 6 [start, end]
                             [2, 5],             # 7 [start, end]
                             [4, 5]], dtype=int) # 8 [start, end]

    # spcs
    spc = np.array([[0, 1, 1, 0],                # [nID, dofX, dofZ, dofPhi]
                    [1, 0, 1, 0]], dtype=int)    # [nID, dofX, dofZ, dofPhi]

    # rbe2
    rbe = np.array([[[6, 1, 1, 1],               # [master, dofX, dofZ, dofPhi]
                     [4, 1, 1, 0],               # [slave, dofX, dofZ, dofPhi]
                     [5, 1, 1, 0]]], dtype=int)  # [slave, dofX, dofZ, dofPhi]

    for i in range(rbe.shape[0]):
        print(RBE2(xz, rbe[i]))

