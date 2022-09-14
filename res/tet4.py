
import numpy as np

# https://inside.mines.edu/~vgriffit/5th_ed/Software/

def shape_fun(points):
  ndim = 3 # dimensions R3
  nod = 4  # number of nodes

  fun = np.zeros((4, points.shape[0]), dtype=float)

  xi = points.T[0]
  eta = points.T[1]
  mu = points.T[2]

  fun[0] = xi
  fun[1] = eta
  fun[2] = mu
  fun[3] = 1.0 - xi - eta - mu

  return fun

def shape_der(points):
  ndim = 3 # dimensions R3
  nod = 4  # number of nodes

  der = np.zeros((points.shape[0], ndim, nod), dtype=float)
  der[:,0,0] = 1.0
  der[:,1,1] = 1.0
  der[:,2,2] = 1.0
  der[:,3,:] = -1.0

  return der

def int_points(nip: int=1):
  if nip == 1:
    s = np.array([0.25, 0.25, 0.25], dtype=float)
    w = 1/6
  elif nip == 4:
    s = np.zeros((4, 3), dtype=float)
    w = np.zeros(4, dtype=float)
    s[0,0] = 0.58541020
    s[0,1] = 0.13819660
    s[0,3] = s[0,1]
    s[1,1] = s[0,0]
    s[1,2] = s[0,1]
    s[1,0] = s[0,1]
    s[2,2] = s[0,0]
    s[2,0] = s[0,1]
    s[2,1] = s[0,1]
    s[3,0] = s[0,1]
    s[3,1] = s[0,1]
    s[3,2] = s[0,1]
    w[0:3] = 0.25 / 6.0
  elif nip == 5:
    s = np.zeros((5, 3), dtype=float)
    w = np.zeros(5, dtype=float)
    s[0,:] = 0.25
    s[1,0] = 0.5
    s[1,1:] = 1. / 6.
    s[2,1] = 0.5
    s[2,2] = 1. / 6.
    s[2,0] = s[2,2]
    s[3,2] = 0.5
    s[3,:2] = 1. / 6.
    s[4,:] = 1. / 6.
    w[0] = -0.8
    w[1:] = 9. / 20.
    w = w / 6.
  else:
    raise(f'Wrong number of inegration points for tetrahedron ({iwp} != ' + '{1, 4, 5})')

  return s, w



def nat_coor(points):
  pass



def tet4(coors: np.ndarray, E: float, nu: float, rho: float, fx: float = 0., fy: float = 0., fz: float = 0.):
    domain = np.array([[1., 0., 0., 0.],
                       [0., 1., 0., 0.],
                       [0., 0., 1., 0.],
                       [0., 0., 0., 1.]], dtype=float)
    print('Domain: {0}\n{1}'.format(domain.shape, domain))

    # gaussian interpolation
    gauss_points = 4
    g_points, g_weights = np.polynomial.legendre.leggauss(gauss_points)
    print(g_points)
    gp = (g_points + 1.) / 2.
    print(gp)
    print(g_weights)
    exit()
    ip = np.zeros((4, 4), dtype=float)
    w = np.zeros(4, dtype=float)
    for i in range(4):
      ip[i,:] = gp[0]
      ip[i,i] = gp[1]
      w[i] = g_weights[1] * 3 * g_weights[0]
    print(ip)

    # shape functions in natural coordinates
    xi = ip.T[0]
    eta = ip.T[1]
    mu = ip.T[2]
    zeta = ip.T[3]
    psi = np.zeros((ip.shape[0], ip.shape[0]), dtype=float)
    for i in range(4):
      psi[i] = (xi * domain[i, 0]) * (eta * domain[i, 1]) * (mu * domain[i, 2])* (zeta * domain[i, 2])
    print(psi)



    exit()
    integration_points = []
    interpolation_weights = []
    gp = (g_points + 1.) / 2.
    ip = []
    ip.append([gp[2], gp[0], gp[2], gp[2]])
    ip.append([gp[1], gp[1], gp[2], gp[2]])
    ip.append([gp[0], gp[2], gp[2], gp[2]])
    ip.append([gp[2], gp[0], gp[1], gp[2]])
    ip.append([gp[1], gp[1], gp[1], gp[2]])
    ip.append([gp[0], gp[2], gp[1], gp[2]])
    ip.append([gp[2], gp[0], gp[0], gp[2]])
    ip.append([gp[1], gp[1], gp[0], gp[2]])
    ip.append([gp[0], gp[2], gp[0], gp[2]])
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
    print('Shape Functions in Global Coordinates: {0}\n{1}'.format(psi_g.shape, psi_g))
    
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
    epsilons = np.array([exx, eyy, ezz, exz, eyz, exy])

    sigmas = (C @ epsilons).T
    epsilons = epsilons.T

def map(coor):
  domain = [[1., 0., 0., 0.],
            [0., 1., 0., 0.],
            [0., 0., 1., 0.],
            [0., 0., 0., 1.]]
  domain = np.array(domain, dtype=float)

  c = np.vstack((coor.T, np.ones(4, dtype=float)))

  T = domain.T @ np.linalg.inv(c)

  print(T)

  return T


if __name__ == '__main__':
    xyz = np.array([[[0., 0., 0.],
                     [1., 0., 0.],
                     [0., 1., 0.],
                     [0., 0., 1.]]], dtype=float)

    # T = map(xyz[0])
    # print(T @ np.array([0.5, 0., 0., 1.]).T)
    # print(T @ np.array([0., 0.5, 0., 1.]).T)

    trans = np.array([[1., 1., 1.],
                     [-1., 1., 1.],
                     [0., 0., 0.]], dtype=float)
    trans[0,:] = trans[0,:]
    trans[2,:] = np.cross(trans[0,:], trans[1,:])
    trans[1,:] = np.cross(trans[2,:], trans[0,:])
    for i in range(3):
        trans[i,:] /= np.linalg.norm(trans[i,:])

    xyz = np.vstack((xyz, [xyz[0] @ trans]))

    for i in range(xyz.shape[0]):
      el = xyz[i]
      T = map(el)

      p = np.ones(4, dtype=float)
      for j in range(3):
        p[j] = (el[0,j] + el[1,j] + el[2,j] + el[3,j]) / 4.

      print(p)
      print(T @ p)

    exit()


    E = 210000.0   # MPa steel
    nu = 0.3       # steel
    rho = 9.81E-9  # steel t/mm3

    fx = 0.  # volumetric continuous load
    fy = 0.  # volumetric continuous load
    fz = 0.  # volumetric continuous load

    for i in range(xyz.shape[0]):
        tet4(xyz[i], E, nu, rho, fx, fy, fz)
