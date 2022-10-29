
import numpy as np

np.set_printoptions(precision=2) # , suppress=True)

def hex8(coors: np.ndarray, E: float, nu: float, rho: float, fx: float = 0., fy: float = 0., fz: float = 0.):
    domain = np.array([[-1., -1., -1.],
                       [1., -1., -1.],
                       [1., 1., -1.],
                       [-1., 1., -1.],
                       [-1., -1., 1.],
                       [1., -1., 1.],
                       [1., 1., 1.],
                       [-1., 1., 1.]], dtype=float)
    print('Domain: {0}\n{1}'.format(domain.shape, domain))

    # gaussian interpolation
    gauss_points = 3
    g_points, g_weights = np.polynomial.legendre.leggauss(gauss_points)
    integration_points = []
    interpolation_weights = []
    for i, xi in enumerate(g_points):
        for j, eta in enumerate(g_points):
            for k, mu in enumerate(g_points):
                integration_points.append([xi, eta, mu])
                interpolation_weights.append(g_weights[i] * g_weights[j] * g_weights[k])
    integration_points = np.array(integration_points, dtype=float)
    interpolation_weights = np.array(interpolation_weights, dtype=float)
    print('Gauss Integration {0}:\n{1}\nWeights:\n{2}'.format(gauss_points, integration_points, interpolation_weights))

    full_domain = np.vstack((domain, integration_points))

    # Shape Functions in Natural Coordinates
    xi = integration_points.T[0]
    eta = integration_points.T[1]
    mu = integration_points.T[2]
    psi = np.zeros((8, integration_points.shape[0]), dtype=float)
    for i in range(8):
        psi[i] = 1/8 * (1 + xi * domain[i, 0]) * (1 + eta * domain[i, 1]) * (1 + mu * domain[i, 2])
    psi = psi.T
    print('Shape Functions: {0}\n{1}'.format(psi.shape, psi))

    # Shape Functions in Global Coordinates
    psi_g = psi @ coors
    print('Shape Functions in '
          'Global Coordinates: {0}\n{1}'.format(psi_g.shape, psi_g))

    # Shape Functions Derivatives in Natural Coordinates
    dpsi = 1 / 8 * np.array([[(eta - 1.0) * (1.0 - mu), (xi - 1) * (1 - mu), -(1 - xi) * (1 - eta)],
                             [(1 - eta) * (1 - mu), (-1 - xi) * (1 - mu), -(1 + xi) * (1 - eta)],
                             [(1 + eta) * (1 - mu), (1 + xi) * (1 - mu), -(1 + xi) * (1 + eta)],
                             [(-1.0 - eta) * (1 - mu), (1 - xi) * (1 - mu), -(1 - xi) * (1 + eta)],
                             [(1 - eta) * (-1 - mu), -(1 - xi) * (1 + mu), (1 - xi) * (1 - eta)],
                             [(1 - eta) * (1 + mu), -(1 + xi) * (1 + mu), (1 + xi) * (1 - eta)],
                             [(1 + eta) * (1 + mu), (1 + xi) * (1 + mu), (1 + xi) * (1 + eta)],
                             [-(1 + eta) * (1 + mu), (1 - xi) * (1 + mu), (1 - xi) * (1 + eta)]])
    dpsi = dpsi.T
    print('Shape Functions Derivatives: {0}\n{1}'.format(dpsi.shape, dpsi))

    # Jacobian Matrix
    jacobi = dpsi @ coors
    print('Jacobian Matrix: {0}\n{1}'.format(jacobi.shape, jacobi))

    # Jacobian Determinants
    d_jacobi = np.linalg.det(jacobi)
    print('Determinant of Jacobian: {0}\n{1}'.format(d_jacobi.shape, d_jacobi))

    # Inverse Jacobian
    i_jacobi = np.linalg.inv(jacobi)
    print('Inverse Jacobian Matrix: {0}\n{1}'.format(i_jacobi.shape, i_jacobi))

    # Shape Function Derivatives in Global Coordinates
    dpsi_g = i_jacobi @ dpsi
    print('Shape Function Derivatives in Global Coordinates: {0}\n{1}'.format(dpsi_g.shape, dpsi_g))

    # Material Stiffness Matrix
    C = E / ((1.0 + nu) * (1.0 - 2.0 * nu)) * np.array([[1.0 - nu, nu, nu, 0.0, 0.0, 0.0],
                                                        [nu, 1.0 - nu, nu, 0.0, 0.0, 0.0],
                                                        [nu, nu, 1.0 - nu, 0.0, 0.0, 0.0],
                                                        [0.0, 0.0, 0.0, (1.0 - 2.0 * nu) / 2.0, 0.0, 0.0],
                                                        [0.0, 0.0, 0.0, 0.0, (1.0 - 2.0 * nu) / 2.0, 0.0],
                                                        [0.0, 0.0, 0.0, 0.0, 0.0, (1.0 - 2.0 * nu) / 2.0]], dtype=float)
    print('Material Stiffness Matrix: {0}\n{1}'.format(C.shape, C))

    # Create Element Stiffness and Mass Matrix
    Ke = np.zeros((domain.size, domain.size), dtype=float)
    Me = np.zeros((domain.size, domain.size), dtype=float)
    Fe = np.zeros((domain.size, 1), dtype=float)
    o = np.zeros(domain.shape[0], dtype=float)

    for i in range(integration_points.shape[0]):  # iterate over Gauss points of domain, * means unpack values
        B = np.array([[*dpsi_g[i, 0, :], *o, *o],
                      [*o, *dpsi_g[i, 1, :], *o],
                      [*o, *o, *dpsi_g[i, 2, :]],
                      [*dpsi_g[i, 2, :], *o, *dpsi_g[i, 0, :]],
                      [*o, *dpsi_g[i, 2, :], *dpsi_g[i, 1, :]],
                      [*dpsi_g[i, 1, :], *dpsi_g[i, 0, :], *o]], dtype=float)
        N = np.array([[*psi[i], *o, *o],
                      [*o, *psi[i], *o],
                      [*o, *o, *psi[i]]], dtype=float)
        F = np.array([[fx], [fy], [fz]], dtype=float)

        Ke += (B.T @ C @ B) * d_jacobi[i] * interpolation_weights[i]
        Me += rho * (N.T @ N) * d_jacobi[i] * interpolation_weights[i]
        Fe += (N.T @ F) * d_jacobi[i] * interpolation_weights[i]

    print('Element Stiffness Matrix: {0}\n{1}'.format(Ke.shape, Ke))
    print('Element Mass Matrix: {0}\n{1}'.format(Me.shape, Me))
    print('Element Volume Force Vector: {0}\n{1}'.format(Fe.shape, Fe))

    # Get Element Stresses and Strains in Nodes and Integration Points
    evaluation_points = full_domain  # integration_points / domain

    # Shape Functions in Natural Coordinates
    xi = evaluation_points.T[0]
    eta = evaluation_points.T[1]
    mu = evaluation_points.T[2]
    psi = np.zeros((8, evaluation_points.shape[0]), dtype=float)
    for i in range(8):
        psi[i] = 1 / 8 * (1 + xi * domain[i, 0]) * (1 + eta * domain[i, 1]) * (1 + mu * domain[i, 2])
    psi = psi.T

    # Shape Functions in Global Coordinates
    psi_g = psi @ coors

    # Shape Functions Derivatives in Natural Coordinates
    dpsi = 1 / 8 * np.array([[(eta - 1.0) * (1.0 - mu), (xi - 1) * (1 - mu), -(1 - xi) * (1 - eta)],
                             [(1 - eta) * (1 - mu), (-1 - xi) * (1 - mu), -(1 + xi) * (1 - eta)],
                             [(1 + eta) * (1 - mu), (1 + xi) * (1 - mu), -(1 + xi) * (1 + eta)],
                             [(-1.0 - eta) * (1 - mu), (1 - xi) * (1 - mu), -(1 - xi) * (1 + eta)],
                             [(1 - eta) * (-1 - mu), -(1 - xi) * (1 + mu), (1 - xi) * (1 - eta)],
                             [(1 - eta) * (1 + mu), -(1 + xi) * (1 + mu), (1 + xi) * (1 - eta)],
                             [(1 + eta) * (1 + mu), (1 + xi) * (1 + mu), (1 + xi) * (1 + eta)],
                             [-(1 + eta) * (1 + mu), (1 - xi) * (1 + mu), (1 - xi) * (1 + eta)]])
    dpsi = dpsi.T

    # Jacobian Matrix
    jacobi = dpsi @ coors

    # Jacobian Determinants
    d_jacobi = np.linalg.det(jacobi)

    # Inverse Jacobian
    i_jacobi = np.linalg.inv(jacobi)

    # Shape Function Derivatives in Global Coordinates
    dpsi_g = i_jacobi @ dpsi

    Ue = np.array([[0., 0., 0.],     # x y z displacements in corner nodes
                   [0., 0., 0.],
                   [0., 0., 0.],
                   [0., 0., 0.],
                   [0., 0., 0.01],
                   [0., 0., 0.01],
                   [0., 0., 0.01],
                   [0., 0., 0.01]], dtype=float)

    du = Ue.T @ np.transpose(dpsi_g, axes=[0, 2, 1])

    exx = du[:, 0, 0]
    eyy = du[:, 1, 1]
    ezz = du[:, 2, 2]
    exy = du[:, 0, 1] + du[:, 1, 0]
    eyz = du[:, 1, 2] + du[:, 2, 1]
    exz = du[:, 0, 2] + du[:, 2, 0]
    epsilons = np.array([exx, eyy, ezz, exy, eyz, exz])

    sigmas = (C @ epsilons).T
    epsilons = epsilons.T


if __name__ == '__main__':
    xyz = np.array([[[0., 0., 0.],
                     [1., 0., 0.],
                     [1., 1., 0.],
                     [0., 1., 0.],
                     [0., 0., 1.],
                     [1., 0., 1.],
                     [1., 1., 1.],
                     [0., 1., 1.]]], dtype=float)

    trans = np.array([[1., 1., 1.],
                     [-1., 1., 1.],
                     [0., 0., 0.]], dtype=float)
    trans[0,:] = trans[0,:]
    trans[2,:] = np.cross(trans[0,:], trans[1,:])
    trans[1,:] = np.cross(trans[2,:], trans[0,:])
    for i in range(3):
        trans[i,:] /= np.linalg.norm(trans[i,:])

    xyz = np.vstack((xyz, [xyz[0] @ trans]))

    E = 210000.0   # MPa steel
    nu = 0.3       # steel
    rho = 9.81E-9  # steel t/mm3

    fx = 0.  # volumetric continuous load
    fy = 0.  # volumetric continuous load
    fz = 0.  # volumetric continuous load

    for i in range(xyz.shape[0]):
        hex8(xyz[i], E, nu, rho, fx, fy, fz)
