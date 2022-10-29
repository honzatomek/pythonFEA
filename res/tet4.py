
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

  der = np.zeros((points.shape[0], nod, ndim), dtype=float)
  der[:,0,0] = 1.0
  der[:,1,1] = 1.0
  der[:,2,2] = 1.0
  der[:,3,:] = -1.0

  # print(der.shape)

  return der

def int_points(nip: int=1):
  if nip == 1:
    s = np.array([[0.25, 0.25, 0.25]], dtype=float)
    w = np.array([1./6.], dtype=float)
    # w = np.array([1.], dtype=float)
  elif nip == 4:
    s = np.zeros((4, 3), dtype=float)
    w = np.zeros(4, dtype=float)
    s[0,0] = 0.58541020
    s[0,1] = 0.13819660
    s[0,2] = s[0,1]
    s[1,1] = s[0,0]
    s[1,2] = s[0,1]
    s[1,0] = s[0,1]
    s[2,2] = s[0,0]
    s[2,0] = s[0,1]
    s[2,1] = s[0,1]
    s[3,0] = s[0,1]
    s[3,1] = s[0,1]
    s[3,2] = s[0,1]
    w[0:4] = 0.25 / 6.0
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

def tet4(coors: np.ndarray, E: float, nu: float, rho: float, fx: float = 0., fy: float = 0., fz: float = 0., gauss_points = 4):
    domain = np.array([[1., 0., 0., 0.],
                       [0., 1., 0., 0.],
                       [0., 0., 1., 0.],
                       [0., 0., 0., 1.]], dtype=float)
    print('Domain: {0}\n{1}'.format(domain.shape, domain))

    # gaussian interpolation
    integration_points = None
    integration_weights = None
    if gauss_points == 1:
        integration_points = np.array([1/4, 1/4, 1/4, 1/4], dtype=float).reshape(1, 4)
        integration_weights = np.array([1 * 1 * 1 * 1], dtype=float) * 1/6

    elif gauss_points == 4:
        a = 0.58541020
        b = 0.13819660
        integration_points = np.array([[a, b, b, b],
                                       [b, a, b, b],
                                       [b, b, a, b],
                                       [b, b, b, a]], dtype=float)
        integration_weights = np.array([1/4, 1/4, 1/4, 1/4], dtype=float) * 1/6

    elif gauss_points == 5:
        integration_points = np.array([[1/4, 1/4, 1/4, 1/4],
                                       [1/2, 1/6, 1/6, 1/6],
                                       [1/6, 1/2, 1/6, 1/6],
                                       [1/6, 1/6, 1/2, 1/6],
                                       [1/6, 1/6, 1/6, 1/2]], dtype=float)
        integration_weights = np.array([-4/5, 9/20, 9/20, 9/20, 9/20], dtype=float) * 1/6

    print('Gauss Integration {0}:\n{1}\nWeights:\n{2}'.format(gauss_points, integration_points, integration_weights))

    full_domain = np.vstack((domain, integration_points))

    # Shape Functions in Natural Coordinates
    xi = integration_points.T[0]
    eta = integration_points.T[1]
    mu = integration_points.T[2]
    zeta = integration_points.T[3] # zeta is not used to enforce (1 - xi - eta - mu - zeta) = 0
    psi = np.zeros((4, integration_points.shape[0]), dtype=float)
    # another way of writing that psi = [xi, eta, mu, zeta]
    # for i in range(4):
    #     psi[i] = (1 + xi * domain[i, 0]) * (1 + eta * domain[i, 1]) * (1 + mu * domain[i, 2]) * (1 + zeta * domain[i, 3])
    psi[0] = xi
    psi[1] = eta
    psi[2] = mu
    psi[3] = 1 - xi - eta - mu  # = zeta
    psi = psi.T
    print('Shape Functions: {0}\n{1}'.format(psi.shape, psi))

    # Shape Functions in Global Coordinates
    psi_g = psi @ coors
    print('Shape Functions in Global Coordinates: {0}\n{1}'.format(psi_g.shape, psi_g))

    # Shape Functions Derivatives in Natural Coordinates
    dpsi = np.zeros((integration_points.shape[0], 4, 3), dtype=float)
    dpsi[:,0,0] = 1
    dpsi[:,1,1] = 1
    dpsi[:,2,2] = 1
    dpsi[:,3,:] = -1
    dpsi = dpsi.transpose((0, 2, 1))
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
    Ke = np.zeros((domain.shape[0] * 3, domain.shape[0] * 3), dtype=float)
    Me = np.zeros((domain.shape[0] * 3, domain.shape[0] * 3), dtype=float)
    Fe = np.zeros((domain.shape[0] * 3, 1), dtype=float)
    # o = np.zeros(domain.shape[0], dtype=float)

    # iterate over Gauss points of domain, * means unpack values
    for i in range(integration_points.shape[0]):
        # this is smart but arranges dofs by component, not by node
        # component-wise dof ordering (x1, .. , y1, .. y4, z1, .. , z4)
        # B = np.array([
        #   [*dpsi_g[i, 0, :], *o, *o],
        #   [*o, *dpsi_g[i, 1, :], *o],
        #   [*o, *o, *dpsi_g[i, 2, :]],
        #   [*dpsi_g[i, 2, :], *o, *dpsi_g[i, 0, :]],
        #   [*o, *dpsi_g[i, 2, :], *dpsi_g[i, 1, :]],
        #   [*dpsi_g[i, 1, :], *dpsi_g[i, 0, :], *o]], dtype=float)

        # dofs ordered by node
        # node wise dof ordering (x1, y1, z1, .. , x4, y4, z4)
        dpx = dpsi_g[i,0,:]
        dpy = dpsi_g[i,1,:]
        dpz = dpsi_g[i,2,:]
        B = np.array(
            [[dpx[0], 0, 0, dpx[1], 0, 0, dpx[2], 0, 0, dpx[3], 0, 0],
             [0, dpy[0], 0, 0, dpy[1], 0, 0, dpy[2], 0, 0, dpy[3], 0],
             [0, 0, dpz[0], 0, 0, dpz[1], 0, 0, dpz[2], 0, 0, dpz[3]],
             [dpy[0], dpx[0], 0, dpy[1], dpx[1], 0, dpy[2], dpx[2], 0, dpy[3], dpx[3], 0],
             [0, dpz[0], dpy[0], 0, dpz[1], dpy[1], 0, dpz[2], dpy[2], 0, dpz[3], dpy[3]],
             [dpz[0], 0, dpx[0], dpz[1], 0, dpx[1], dpz[2], 0, dpx[2], dpz[3], 0, dpx[3]]],
             dtype=float)

        # this is smart but arranges dofs by component, not by node
        # component-wise dof ordering (x1, .. , y1, .. y4, z1, .. , z4)
        # N = np.array([
        #   [*psi[i], *o, *o],
        #   [*o, *psi[i], *o],
        #   [*o, *o, *psi[i]]],
        #   dtype=float)

        # node wise dof ordering (x1, y1, z1, .. , x4, y4, z4)
        N = np.array([[psi[i, 0], 0, 0, psi[i, 1], 0, 0, psi[i, 2], 0, 0, psi[i, 3], 0, 0],
                      [0, psi[i, 0], 0, 0, psi[i, 1], 0, 0, psi[i, 2], 0, 0, psi[i, 3], 0],
                      [0, 0, psi[i, 0], 0, 0, psi[i, 1], 0, 0, psi[i, 2], 0, 0, psi[i, 3]]],
                      dtype=float)

        F = np.array([[fx], [fy], [fz]], dtype=float)

        print((B.T @ C @ B).shape)
        print(Ke.shape)
        Ke += (B.T @ C @ B) * d_jacobi[i] * integration_weights[i]
        Me += rho * (N.T @ N) * d_jacobi[i] * integration_weights[i]
        Fe += (N.T @ F) * d_jacobi[i] * integration_weights[i]

    print('Element Stiffness Matrix: {0}\n{1}'.format(Ke.shape, Ke))
    print('Element Mass Matrix: {0}\n{1}'.format(Me.shape, Me))
    print('Element Volume Force Vector: {0}\n{1}'.format(Fe.shape, Fe))

    # Get Element Stresses and Strains in Nodes and Integration Points
    evaluation_points = full_domain  # integration_points / domain


    Ue = np.array([[0., 0., 0.],     # x y z displacements in corner nodes
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

# transform global coordinates to natural
def transform(points):
    T = map(points)

    p = np.ones(4, dtype=float)
    for j in range(3):
      p[j] = (points[0,j] + points[1,j] + points[2,j] + points[3,j]) / 4.

    print(p)
    n = T @ p
    print(n)
    p = np.linalg.inv(T) @ n
    print(p)

def trans_mat_from_3_points(origin, x_axis, xy_plane):
    T = np.zeros((3, 3), dtype=float)
    T[0] = x_axis - origin
    T[1] = xy_plane - origin
    T[2,:] = np.cross(T[0,:], T[1,:])
    T[1,:] = np.cross(T[2,:], T[0,:])
    for i in range(3):
        T[i,:] /= np.linalg.norm(T[i,:])

    return T

def tet4_2(coor, E, nu, rho, fx, fy, fz, ipn: int=1):
    domain = np.array([[1., 0., 0., 0.],
                       [0., 1., 0., 0.],
                       [0., 0., 1., 0.],
                       [0., 0., 0., 1.]], dtype=float)
    print('Domain: {0}\n{1}'.format(domain.shape, domain))
    ip, w = int_points(ipn)
    print('Gauss Integration {0}:\n{1}\nWeights:\n{2}'.format(ipn, ip, w))
    psi = shape_fun(ip).T
    print('Shape Functions: {0}\n{1}'.format(psi.shape, psi))
    psi_g = psi @ coor
    print('Shape Functions in Global Coordinates: {0}\n{1}'.format(psi_g.shape, psi_g))
    dpsi = shape_der(ip).transpose((0,2,1))
    print('Shape Functions Derivatives: {0}\n{1}'.format(dpsi.shape, dpsi))
    # print(coor.shape)
    jacobi = []
    for i in range(dpsi.shape[0]):
        jacobi.append(dpsi[i] @ coor)
    jacobi = np.array(jacobi, dtype=float)
    print('Jacobian Matrix: {0}\n{1}'.format(jacobi.shape, jacobi))
    d_jacobi = np.linalg.det(jacobi)
    print('Determinant of Jacobian: {0}\n{1}'.format(d_jacobi.shape, d_jacobi))
    i_jacobi = np.linalg.inv(jacobi)
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
    Ke = np.zeros((coor.size, coor.size), dtype=float)
    Me = np.zeros((coor.size, coor.size), dtype=float)
    Fe = np.zeros((coor.size, 1), dtype=float)
    o = np.zeros(coor.shape[0], dtype=float)

    for i in range(ip.shape[0]):  # iterate over Gauss points of domain, * means unpack values
        # this is smart but arranges dofs by component, not by node
        # component-wise dof ordering (x1, .. , y1, .. y4, z1, .. , z4)
        # B = np.array([
        #   [*dpsi_g[i, 0, :], *o, *o],
        #   [*o, *dpsi_g[i, 1, :], *o],
        #   [*o, *o, *dpsi_g[i, 2, :]],
        #   [*dpsi_g[i, 2, :], *o, *dpsi_g[i, 0, :]],
        #   [*o, *dpsi_g[i, 2, :], *dpsi_g[i, 1, :]],
        #   [*dpsi_g[i, 1, :], *dpsi_g[i, 0, :], *o]], dtype=float)

        # dofs ordered by node
        # node wise dof ordering (x1, y1, z1, .. , x4, y4, z4)
        dpx = dpsi_g[i,0,:]
        dpy = dpsi_g[i,1,:]
        dpz = dpsi_g[i,2,:]
        B = np.array(
            [[dpx[0], 0, 0, dpx[1], 0, 0, dpx[2], 0, 0, dpx[3], 0, 0],
             [0, dpy[0], 0, 0, dpy[1], 0, 0, dpy[2], 0, 0, dpy[3], 0],
             [0, 0, dpz[0], 0, 0, dpz[1], 0, 0, dpz[2], 0, 0, dpz[3]],
             [dpy[0], dpx[0], 0, dpy[1], dpx[1], 0, dpy[2], dpx[2], 0, dpy[3], dpx[3], 0],
             [0, dpz[0], dpy[0], 0, dpz[1], dpy[1], 0, dpz[2], dpy[2], 0, dpz[3], dpy[3]],
             [dpz[0], 0, dpx[0], dpz[1], 0, dpx[1], dpz[2], 0, dpx[2], dpz[3], 0, dpx[3]]],
             dtype=float)

        # this is smart but arranges dofs by component, not by node
        # component-wise dof ordering (x1, .. , y1, .. y4, z1, .. , z4)
        # N = np.array([
        #   [*psi[i], *o, *o],
        #   [*o, *psi[i], *o],
        #   [*o, *o, *psi[i]]],
        #   dtype=float)

        # node wise dof ordering (x1, y1, z1, .. , x4, y4, z4)
        N = np.array([[psi[i, 0], 0, 0, psi[i, 1], 0, 0, psi[i, 2], 0, 0, psi[i, 3], 0, 0],
                      [0, psi[i, 0], 0, 0, psi[i, 1], 0, 0, psi[i, 2], 0, 0, psi[i, 3], 0],
                      [0, 0, psi[i, 0], 0, 0, psi[i, 1], 0, 0, psi[i, 2], 0, 0, psi[i, 3]]],
                      dtype=float)

        F = np.array([[fx], [fy], [fz]], dtype=float)

        # print(ip)
        Ke += (B.T @ C @ B) * d_jacobi[i] * w[i]
        Me += rho * (N.T @ N) * d_jacobi[i] * w[i]
        Fe += (N.T @ F) * d_jacobi[i] * w[i]

    print('Element Stiffness Matrix: {0}\n{1}'.format(Ke.shape, Ke))
    print('Element Mass Matrix: {0}\n{1}'.format(Me.shape, Me))
    print('Element Volume Force Vector: {0}\n{1}'.format(Fe.shape, Fe))

if __name__ == '__main__':
  # xyz = np.array([[
  #   [0., 0., 0.],
  #   [1., 0., 0.],
  #   [0., 1., 0.],
  #   [0., 0., 1.]]], dtype=float)

  xyz = np.array([[
    [2., 3., 4.],
    [6., 3., 2.],
    [2., 5., 1.],
    [4., 3., 6.]]], dtype=float)

  # T = trans_mat_from_3_points(np.array([0., 0., 0.]),
  #     np.array([1., 1., 1.]),
  #     np.array([1., -1., 1.]))
  # xyz = np.vstack((xyz, [xyz[0] @ T]))

  # for i in range(xyz.shape[0]):
  #   transform(xyz[i])

  # E, nu, rho = 210000., 0.3, 7.85E-9 # steel
  E, nu, rho = 96., 1./3., 7.85E-9 # steel
  fx, fy, fz = 0., 0., 0. # volumetric continuous load

  for i in range(xyz.shape[0]):
    tet4(xyz[i], E, nu, rho, fx, fy, fz, 4)
