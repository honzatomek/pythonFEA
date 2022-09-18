#!/usr/bin/python3

import numpy as np

class Node:
    def __init__(self, id: int, coors: np.ndarray):
        self.id = id
        self.x = coors[0]
        self.y = coors[1]
        self.z = coors[2]

    @property
    def coors(self):
        return np.array([self.x, self.y, self.z], dtype=float)

class Property:
    def __init__(self, id: int, mat, geom = None):
        self.id = id
        self.material = mat
        self.geometry = geom

class Material:
    def __init__(self, id: int, E, nu, rho):
        self.id = id
        self.E = E
        self.nu = nu
        self.rho = rho

    # material stiffness matrix
    @property
    def Emat(self):
        E = self.E
        nu = self.nu
        C = E / ((1.0 + nu) * (1.0 - 2.0 * nu)) * np.array([[1.0 - nu, nu, nu, 0.0, 0.0, 0.0],
                                                            [nu, 1.0 - nu, nu, 0.0, 0.0, 0.0],
                                                            [nu, nu, 1.0 - nu, 0.0, 0.0, 0.0],
                                                            [0.0, 0.0, 0.0, (1.0 - 2.0 * nu) / 2.0, 0.0, 0.0],
                                                            [0.0, 0.0, 0.0, 0.0, (1.0 - 2.0 * nu) / 2.0, 0.0],
                                                            [0.0, 0.0, 0.0, 0.0, 0.0, (1.0 - 2.0 * nu) / 2.0]], dtype=float)
        return C

class Element:
    # interpolation points
    IP = None
    # interpolation weights
    IW = None
    domain = None

    def __init__(self, id: int, nodes, property):
        self.id = id
        self.nodes = nodes
        self.coors = np.array([n.coors for n in self.nodes], dtype=float)
        self.property = property

    # shape functions in natural coordinates - individual for each element implementation
    def shape_fun(self):
        psi = np.array([], dtype=float)
        return psi

    # shape function derivatives in natural coordinates - individual for each element implementation
    def shape_fun_derivatives(self, IP):
        dpsi = np.array([], dtype=float)
        return dpsi

    # Jacobian matrix
    def J(self, dpsi):
        jacobi = dpsi @ self.coors
        d_jacobi = np.linalg.det(jacobi)
        i_jacobi = np.linalg.inv(jacobi)
        return jacobi, d_jacobi, i_jacobi

    # shape function derivatives in global coordinates
    def shape_fun_derivatives_global(self, i_jacobi, dpsi):
        return i_jacobi @ dpsi

    # Element Stiffness Matrix
    @property
    def K(self):
        dpsi = self.shape_fun_derivatives(type(self).IP)
        jacobi, d_jacobi, i_jacobi = self.J(dpsi)
        dpsi_g = self.shape_fun_derivatives_global(i_jacobi, dpsi)
        n = type(self).IP.shape[0]
        Ke = np.zeros((domain.size, domain.size), dtype=float)
        C = self.property.material.Emat
        for i in range(type(self).IP.shape[0]):
            B = np.array([np.array([[dpsi_g[i, 0, j], 0., 0.] for j in range(n)], dtype=float).flatten(),
                          np.array([[0., dpsi_g[i, 1, j], 0.] for j in range(n)], dtype=float).flatten(),
                          np.array([[0., 0., dpsi_g[i, 2, j]] for j in range(n)], dtype=float).flatten(),
                          np.array([[dpsi_g[i, 2, j], 0., dpsi_g[i, 0, j]] for j in range(n)], dtype=float).flatten(),
                          np.array([[0., dpsi_g[i, 2, j], dpsi_g[i, 1, j]] for j in range(n)], dtype=float).flatten(),
                          np.array([[dpsi_g[i, 1, j], 0., dpsi_g[i, 0, j]] for j in range(n)], dtype=float).flatten()], dtype=float)

            Ke += (B.T @ C @ B) * d_jacobi[i] * type(self).IW[i]

        return Ke


