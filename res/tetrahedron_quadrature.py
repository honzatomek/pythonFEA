#!/usr/bin/python3

import numpy as np
from math import sqrt

DEBUG = False


def dprint(message):
    if DEBUG:
        print(message)
    else:
        pass


def rquad(N: int, k: int) -> np.ndarray:
    k1 = k + 1
    dprint(f'k1 = {k1}')
    k2 = k + 2
    dprint(f'k2 = {k2}')

    n = np.linspace(1, N, N, dtype=int)
    dprint(f'n = {n}')
    nnk = 2 * n + k
    dprint(f'nnk = {nnk}')
    A = np.array([k / k2, *np.repeat(k ** 2, N) / (nnk * (nnk + 2))], dtype=float).reshape(1, -1)
    dprint(f'A = {A}')

    n = np.linspace(2, N, N - 1, dtype=int)
    dprint(f'n = {n}')
    nnk = nnk[n-1]
    dprint(f'nnk = {nnk}')
    B1 = 4 * k1 / (k2 ** 2 * (k + 3))
    dprint(f'B1 = {B1}')

    nk =  n + k
    dprint(f'nk = {nk}')
    nnk2 = nnk * nnk
    dprint(f'nnk2 = {nnk2}')
    B = ((4 * (n * nk) ** 2) / (nnk2 * nnk2 - nnk2)).reshape(1, -1)
    dprint(f'B = {B}')

    ab = np.hstack([A.T, np.vstack([(2 ** k1)/k1, B1, B.T])])
    dprint(f'ab = {ab}')

    s = np.sqrt(ab[1:N,1]).flatten()
    dprint(f's = {s}')

    X, V = np.linalg.eig(np.diag(ab[:N,0].flatten(), 0) + np.diag(s, -1) + np.diag(s, 1))
    dprint(f'V, = {V}')
    dprint(f'X, = {X}')

    I = X.argsort()
    dprint(f'I = {I}')
    X = X[I]
    dprint(f'X = {X}')
    V = V[:,I]
    dprint(f'V = {V}')

    # Grid points
    x = ((X + 1) / 2).reshape(-1, 1)
    dprint(f'x = {x}')

    # Quadrature weights
    w = (0.5 ** k1 * ab[0,1] * (V[0, :].T ** 2)).reshape(-1, 1)
    dprint(f'w = {w}')

    return x, w


def tetrahedron_quadrature(vertices: np.ndarray) -> np.ndarray:
    """
    Construct Gauss points and weights for a tetrahedral domain with vertices
    specified by the 4x3 matrix vertices. Where each row contains the (x,y,z) for
    a vertex.

    Sample usage:

     Suppose f(x,y,z)=x^2+y^2+z^2. Then let's integrate this over a regular
     tetrahedron.

     >> vert = np.array([1/sqrt(3) 0 0; -sqrt(3)/6,1/2,0;-sqrt(3)/6,-1/2,0;0 0 sqrt(6)/3])
     >> X, Y, Z, W = tetrahedron_quadrature(vert)
     >> F = X**2 + Y**2 + Z**2
     >> Q = W.T * F

     Written by: Greg von Winckel
     Contact: gregvw(at)math(dot)unm(dot)edu
     http://math.unm.edu/~gregvw
    """
    N = vertices.shape[0]
    q1, w1 = rquad(N, 2)
    q2, w2 = rquad(N, 1)
    q3, w3 = rquad(N, 0)

    [q1, q2, q3] = np.meshgrid(q1, q2, q3)
    q1 = q1.flatten().reshape(-1,1)
    q2 = q2.flatten().reshape(-1,1)
    q3 = q3.flatten().reshape(-1,1)
    dprint(f'q1 = {q1}')
    dprint(f'q2 = {q2}')
    dprint(f'q3 = {q3}')

    x = 1 - q1
    y = (1 - q2) * q1
    z = q1 * q2 * q3
    dprint(f'x = {x}')
    dprint(f'y = {y}')
    dprint(f'z = {z}')

    w = ((w2 @ w1.T).reshape(N**2, 1) @ w3.T).reshape(N**3, 1)
    dprint(f'w = {w}')

    c = np.array([[1, 0, 0, 0],
                  [-1, 1, 0, 0],
                  [-1, 0, 1, 0],
                  [-1, 0, 0, 1]], dtype=float) @ vert
    dprint(f'c = {c}')

    W = np.abs(np.linalg.det(c[1:4,:])) * w
    dprint(f'W = {W}')

    # change of coordinates
    XYZ = np.hstack([np.ones((N ** 3, 1)), x, y, z]) @ c
    dprint(f'XYZ = {XYZ}')

    return XYZ[:,0].reshape(-1,1), XYZ[:,1].reshape(-1,1), XYZ[:,2].reshape(-1,1), W


if __name__ == '__main__':
    vert = np.array([[1/sqrt(3), 0, 0],
                     [-sqrt(3)/6, 1/2, 0],
                     [-sqrt(3)/6, -1/2, 0],
                     [0, 0, sqrt(6)/3]], dtype=float)
    X, Y, Z, W = tetrahedron_quadrature(vert)

    F = X**2 + Y**2 + Z**2
    Q = W.T @ F
    print(Q)

