#!/usr/bin/python

import numpy as np
import numpy.matlib

from math import sqrt

def tetraquad(N = None,vert = None):
    ###########################################################################

    # tetraquad.m - Gaussian Quadrature for a tetrahedron

    # Construct Gauss points and weights for a tetrahedral domain with vertices
# specified by the 4x3 matrix vert. Where each row contains the (x,y,z) for
# a vertex.

    # Sample usage:

    # Suppose f(x,y,z)=x^2+y^2+z^2. Then let's integrate this over a regular
# tetrahedron.

    # >>vert=[1/sqrt(3) 0 0; -sqrt(3)/6,1/2,0;-sqrt(3)/6,-1/2,0;0 0 sqrt(6)/3];
# >>[X,Y,Z,W]=tetraquad(4,vert);
# >>F=X.^2+Y.^2+Z.^2;
# >>Q=W'*F;

    # Written by: Greg von Winckel
# Contact: gregvw(at)math(dot)unm(dot)edu
# http://math.unm.edu/~gregvw

    ###########################################################################
# gauss points
    q1,w1 = rquad(N,2)
    q2,w2 = rquad(N,1)
    q3,w3 = rquad(N,0)
    q1,q2,q3 = np.meshgrid(q1,q2,q3)
    q1 = q1
    q2 = q2
    q3 = q3
    x = 1 - q1
    y = np.multiply((1 - q2),q1)
    z = np.multiply(np.multiply(q1,q2),q3)
    w = reshape(reshape(w2 * np.transpose(w1),N ** 2,1) * np.transpose(w3),N ** 3,1)
    c = np.array([[1,0,0,0],[- 1,1,0,0],[- 1,0,1,0],[- 1,0,0,1]]) * vert
    W = np.abs(np.linalg.det(c[np.arange(2,4+1),:])) * w
    # Change of coordinates
    XYZ = np.array([np.ones((N ** 3,1)),x,y,z]) * c
    X = XYZ[:,1]
    Y = XYZ[:,2]
    Z = XYZ[:,3]

def rquad(N = None,k = None):
    k1 = k + 1
    k2 = k + 2
    n = np.arange(1,N+1)
    nnk = 2 * n + k
    A = np.array([k / k2,np.matlib.repmat(k ** 2,1,N) / (np.multiply(nnk,(nnk + 2)))])
    n = np.arange(2,N+1)
    nnk = nnk[n-1]
    B1 = 4 * k1 / (k2 * k2 * (k + 3))
    nk = n + k
    nnk2 = np.multiply(nnk,nnk)
    B = 4 * (np.multiply(n,nk)) ** 2.0 / (np.multiply(nnk2,nnk2) - nnk2)
    ab = np.array([np.transpose(A),np.array([[(2 ** k1) / k1],[B1],[np.transpose(B)]])])
    print(ab)
    s = np.sqrt(ab[np.arange(2,N+1),2])
    V,X = np.linalg.eig(np.diag(ab[np.arange(1,N+1),1],0) + diag(s,- 1) + diag(s,1))
    X,I = __builtint__.sorted(diag(X))
    # Grid points
    x = (X + 1) / 2
    # Quadrature weights
    w = (1 / 2) ** (k1) * ab(1,2) * np.transpose(V(1,I)) ** 2
    return X,Y,Z,W

if __name__ == '__main__':
    # Suppose f(x,y,z)=x^2+y^2+z^2. Then let's integrate this 
    # over a regular tetrahedron.

    print('f(x,y,z)=x^2+y^2+z^2')
    vert = np.array([[1/sqrt(3), 0, 0],
                     [-sqrt(3)/6, 1/2, 0],
                     [-sqrt(3)/6, -1/2, 0],
                     [0, 0, sqrt(6)/3]], dtype=float)
    print(vert)
    X, Y, Z, W = tetraquad(4, vert)
    F = X ** 2 + Y ** 2 + Z ** 2
    Q = W * F
    print(Q)
# >>[X,Y,Z,W]=tetraquad(4,vert);
# >>F=X.^2+Y.^2+Z.^2;
# >>Q=W'*F;
