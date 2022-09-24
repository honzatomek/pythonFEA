#!/usr/bin/python3
import numpy as np

# from https://math.stackexchange.com/questions/721076/help-with-using-the-runge-kutta-4th-order-method-on-a-system-of-2-first-order-od
# eq.:
#      d2y/dx2 + dy/dx -6y = 0, with y(0) = 3 and y'(0) = 0
# this translates to:
#      dy/dx = z
#      dz/dx = 6y - z

def  RK4():
    dp = 0 # int
    m = 2  # order of ODE
    Y = np.zeros(m, dtype=float)
    a = b = x = h = 0.
    N = i = 0

    # Number of steps
    N = 10

    # initial x (e.g. time)
    a = 0.
    x = a

    # final x (e.g. time)
    b = 1.

    # step size (e.g. time)
    h = (b - a) / N

    # initial conditions
    Y[0] = 3. # y[0]
    Y[1] = 1. # y'[0]

    # iterate N times
    for i in range(N):
        Y = iterate(x, Y, h, m)
        x += h

    return Y


# function f computes the vector f
def f(x: float, Yvec: np.array):
    fvec = np.zeros(Yvec.shape)

    fvec[0] = Yvec[1] # z
    fvec[1] = 6 * Yvec[0] - Yvec[1] # 6y-z

    return fvec


# function iterate computes Y(t_n+1)
def iterate(x: float, Y_n: np.array, h: float, m: float):
    Y_nplus1 = np.zeros(Y_n.shape, dtype=float)
    k1 = np.zeros(m, dtype=float)
    k2 = np.zeros(m, dtype=float)
    k3 = np.zeros(m, dtype=float)
    k4 = np.zeros(m, dtype=float)

    k1 = h * f(x, Y_n)
    k2 = h * f(x + h/2, Y_n + k1/2)
    k3 = h * f(x + h/2, Y_n + k2/2)
    k4 = h * f(x + h, Y_n + k3)

    Y_nplus1 = Y_n + (k1 + 2*k2 + 2*k3 + k4)/6

    return Y_nplus1

# The first value is y(1), the second z(1),
# correct to the third decimal point with only ten steps.

if __name__ == '__main__':
  print(RK4())

