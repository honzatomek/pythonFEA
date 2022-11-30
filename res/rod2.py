#!/usr/bin/python3

import numpy as np
np.set_printoptions(precision=6, suppress=True)

def rod2(xyz, E, A, rho, fx, fy, fz, gauss_points=1):
    print('\nStarting ROD element generation:\n')
    coors = xyz.reshape(-1, 1)
    print('-------------------------------------------------------------- Input')
    print('coors =\n{0}'.format(coors))
    print(f'{A = } mm2\n{rho = } t/mm2\n{fx = } N/mm\n{fy = } N/mm\n{fz = } N/mm' +
          f'\n{gauss_points = }\n')

    domain = np.array([[-1],
                       [ 1]], dtype=float)
    print('Domain: {0}\n{1}\n'.format(domain.shape, domain))

    print('----------------------------------------------------- Gauss-Legendre')
    # gauss-legendre integration
    ip, iw = np.polynomial.legendre.leggauss(gauss_points)
    print('Gauss-Legendre Integration\nip = {0}\n{1}\n'.format(ip, iw))

    full_domain = np.vstack((domain, ip.reshape(-1, 1)))

    print('---------------------------------------------------- Shape Functions')
    # shape functions in natural coordinates
    xi = ip.T
    print('xi = {0}\n'.format(xi))
    psi = []
    for i in range(xi.shape[0]):
        psi.append(np.array([[1/2 - xi[i]/2, 0, 0, 1/2 + xi[i]/2, 0, 0],
                            [0, 1/2 - xi[i]/2, 0, 0, 1/2 + xi[i]/2, 0],
                            [0, 0, 1/2 - xi[i]/2, 0, 0, 1/2 + xi[i]/2]], dtype=float))
    psi = np.array(psi)
    # psi = psi.transpose(0, 2, 1)
    print('Shape Functions: {0}\n{1}\n'.format(psi.shape, psi))

    # integration points in global coordinates
    psi_g = psi @ coors
    print('Integration Points in '
          'Global Coordinates: {0}\n{1}\n'.format(psi_g.shape, psi_g))

    # shape function derivatives in natural coordinates
    dpsi = []
    for i in range(xi.shape[0]):
        dpsi.append(np.array([[-1/2, 0, 0, 1/2, 0, 0],
                              [0, -1/2, 0, 0, 1/2, 0],
                              [0, 0, -1/2, 0, 0, 1/2]], dtype=float))
    dpsi = np.array(dpsi)
    print('Shape Functions Derivatives: {0}\n{1}\n'.format(dpsi.shape, dpsi))

    print('---------------------------------------------------------- Jacobians')
    # Jacobian Matrix
    jacobi = dpsi @ coors
    print('Jacobian Matrix: {0}\n{1}\n'.format(jacobi.shape, jacobi))

    # Jacobian Determinants
    d_jacobi = []
    for i in range(xi.shape[0]):
        d_jacobi.append(np.linalg.norm(jacobi[i]))
    d_jacobi = np.array(d_jacobi)
    print('Determinant of Jacobian: {0}\n{1}\n'.format(d_jacobi.shape, d_jacobi))

    # Inverse Jacobian
    i_jacobi = np.linalg.pinv(jacobi)
    print('Inverse Jacobian Matrix: {0}\n{1}\n'.format(i_jacobi.shape, i_jacobi))

    # Shape Function Derivatives in Global Coordinates
    dpsi_g = i_jacobi @ dpsi
    print('Shape Function Derivatives in Global Coordinates: {0}\n{1}\n'.format(dpsi_g.shape, dpsi_g))

    # material stiffness
    D = np.array([[E * A]], dtype=float)
    print('Material Stiffness Matrix: {0}\n{1}\n'.format(D.shape, D))

    # create element stiffness and mass matrix
    Ke = np.zeros((domain.shape[0] * 3, domain.shape[0] * 3), dtype=float)
    Me = np.zeros((domain.shape[0] * 3, domain.shape[0] * 3), dtype=float)
    Fe = np.zeros((domain.shape[0] * 3, 1), dtype=float)

    print('---------------------------------------------------- System Matrices')
    # iterate over integration points
    F = np.array([[fx], [fy], [fz]], dtype=float)
    for i in range(xi.shape[0]):
        B = dpsi_g[i]
        N = psi[i]

        Ke += (B.T @ D @ B) * d_jacobi[i] * iw[i]
        Me += rho * (N.T @ N) * d_jacobi[i] * iw[i]
        Fe += (N.T @ F) * d_jacobi[i] * iw[i]

    print('Element Stiffness Matrix: {0}\n{1}\n'.format(Ke.shape, Ke))
    print('Element Mass Matrix: {0}\n{1}\n'.format(Me.shape, Me))
    print('Element Volume Force Vector: {0}\n{1}\n'.format(Fe.shape, Fe))

    # return Ke, Me, Fe

    print('------------------------------------------------------ Displacements')
    # xyz displacements at end nodes
    u = np.array([0.,
                  0.,
                  0.,
                  .001,
                  .001,
                  .001], dtype=float).reshape(-1, 1)
    print('Displacements {0}\n{1}\n'.format(u.shape, u))

    print('------------------------------------------------------------ Strains')
    eps = []
    for i in range(xi.shape[0]):
        eps.append(dpsi_g[i] @ u)
    eps = np.array(eps)
    print('Strains {0}\n{1}\n'.format(eps.shape, eps))

    print('----------------------------------------------------------- Stresses')
    sig = D @ eps
    print('Stresses {0}\n{1}\n'.format(sig.shape, sig))



if __name__ == '__main__':
    coor = np.array([[    0, 0., 0.],
                     [1000., 0., 0.]], dtype=float)

    E = 210000.0   # MPa steel
    A = 200.      # area mm2
    rho = 9.81E-9  # steel t/mm3

    fx = 1.  # volumetric continuous load
    fy = 0.  # volumetric continuous load
    fz = 1.  # volumetric continuous load

    rod2(coor, E, A, rho, fx, fy, fz, 1)

