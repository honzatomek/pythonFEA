#!/usr/bin/python3

import numpy as np
import matplotlib.pyplot as plt


TOL = 1.e-7

def disp_velo_acce(h, y0, y, m, c, k, f):
  u = y0[0] + y[1] * h
  v = y0[1] + y[2] * h
  a = (f - c * v - k * u) / m

  return np.array([u, v, a])


def _eval_rk4(h, y, k1, k2, k3, k4, m, c, k, f):
  u = y[0] + h/6 * (k1[1] + 2 * (k2[1] + k3[1]) + k4[1])
  v = y[1] + h/6 * (k1[2] + 2 * (k2[2] + k3[2]) + k4[2])
  a = disp_velo_acce(0, [u, v, 0.], [u, v, 0.], m, c, k, f)[2]

  return np.array([u, v, a])


def _eval_rk45(h, y, k1, k2, k3, k4, k5, k6,  m, c, k, f):
  u = y[0] + 16./135. * k1[1] + 0 * k2[1] + 6656./12825. * k3[1] + 28561./56430. * k4[1] - 9./50. * k5[1] + 2./55. * k6[1]
  v = y[1] + 16./135. * k1[2] + 0 * k2[2] +  6656./12825. * k3[2] + 28561./56430. * k4[2] - 9./50. * k5[2] + 2./55. * k6[2]
  du5 = y[0] + 25./216. * k1[1] + 0 * k2[1] + 1408./2565. * k3[1] + 2197./4104. * k4[1] - 1./5. * k5[1] + 0. * k6[1]
  dv5 = y[1] + 25./216. * k1[2] + 0 * k2[2] + 1408./2565. * k3[2] + 2197./4104. * k4[2] - 1./5. * k5[2] + 0. * k6[2]
  a = disp_velo_acce(0, [u, v, 0.], [u, v, 0.], m, c, k, f)[2]
  da5 = disp_velo_acce(0, [du5, dv5, 0.], [du5, dv5, 0.], m, c, k, f)[2]

  err = 1.e-2 * TOL + max(abs(da5-a), 0.)

  return [u, v, a, err]


def _rk4(h, y, m, c, k, f):
  k1 = disp_velo_acce(0., y, y, m, c, k, f[0])
  k2 = disp_velo_acce(h/2., y, k1, m, c, k, (f[1]+f[0])/2.)
  k3 = disp_velo_acce(h/2., y, k2, m, c, k, (f[1]+f[0])/2.)
  k4 = disp_velo_acce(h, y, k3, m, c, k, f[1])

  return _eval_rk4(h, y, k1, k2, k3, k4, m, c, k, f[1])


def _rk45(h, y, m, c, k, f):
  k1 = disp_velo_acce(0., y, y, m, c, k, f[0])
  k2 = disp_velo_acce(h/4., y, k1, m, c, k, f[0] + (f[1]-f[0])/2.)
  k3 = disp_velo_acce(h/8., y, 3./32. * k1 + 9./32. * k2, m, c, k, f[0] + 3./8.*(f[1]-f[0]))
  k4 = disp_velo_acce(12.*h/13., y, 1932./2197. * k1 - 7200./2197. * k2 + 7296./2197. * k3, m, c, k, f[0]+12./13.*(f[1]-f[0]))
  k5 = disp_velo_acce(h, y, 439./216. * k1 - 8. * k2 + 3680./513. * k3 - 845./4104. * k4, m, c, k, f[0]+12./13.*(f[1]-f[0]))
  k6 = disp_velo_acce(h/2., y, - 8./27. * k1 + 2. * k2 - 3544./2565. * k3 + 1859./4104. * k4 - 11./40. * k5, m, c, k, f[0]+1./2.*(f[1]-f[0]))

  u, v, a, err = _eval_rk45(h, y, k1, k2, k3, k4, k5, k6, m, c, k, f[1])

  if err > TOL:
    ndiv = 10
    u, v, a = y
    for i in range(2):
      u, v, a = _rk45(h/ndiv, [u, v, a], m, c, k, (f[0], f[0] + 1/ndiv*(f[1]-f[0])))

  return [u, v, a]


def rk4vibration(t, u, v, a, m, c, k, f):
  nmodes = u.shape[0]
  N = u.shape[1]
  for j in range(N-1):
    dt = t[j+1] - t[j]
    for i in range(nmodes):
      ui, vi, ai = _rk4(dt, [u[i,j], v[i,j], a[i,j]], m[i], c[i], k[i], (f[i,j], f[i,j+1]))
      u[i,j+1] = ui
      v[i,j+1] = vi
      a[i,j+1] = ai

  return u, v, a


def func(m, c, k, f):
  return lambda u, v: (f - c * v - k * u) / m


if __name__ == '__main__':
  k = np.array([10.])   # Hz
  m = np.array([0.001]) # t
  c = np.array([0.02])  # x 100 %
  f = np.array([100.])  # N

  # a_f = func(m[0], c[0], k[0], f[0])
  # print(a_f(10., 100.))

  t = np.linspace(0., 1., 10000)
  u = np.zeros((1, t.shape[0]), dtype=float)
  v = np.zeros((1, t.shape[0]), dtype=float)
  a = np.zeros((1, t.shape[0]), dtype=float)
  f = np.zeros((1, t.shape[0]), dtype=float)

  u[0,0] = 10.

  u, v, a = rk4vibration(t, u, v, a, m, c, k, f)

  fig, ax = plt.subplots()
  l = ax.plot(t, u[0,:])
  plt.show()

