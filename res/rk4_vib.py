#!/usr/bin/python3

import numpy as np


def disp_velo_acce(h, y0, y, m, c, k, f):
  u = y0[0] + y[1] * h
  v = y0[1] + y[2] * h
  a = (f - c * v - k * u) / m

  return [u, v, a]


def _eval_rk4(h, y, k1, k2, k3, k4, m, c, k, fl):
  u = y[0] + h/6 * (k1[1] + 2 * (k2[1] + k3[1]) + k4[1])
  v = y[1] + h/6 * (k1[2] + 2 * (k2[2] + k3[2]) + k4[2])
  a = disp_velo_acce(0, [u, v, 0.], [u, v, 0.], m, c, k, f)[2]

  return [u, v, a]


def _rk4(h, y, m, c, k, f):
  k1 = disp_velo_acce(0., y, y, m, c, k, f[0])
  k2 = disp_velo_acce(h/2., y, k1, m, c, k, (f[1]+f[0])/2.)
  k3 = disp_velo_acce(h/2., y, k2, m, c, k, (f[1]+f[0])/2.)
  k4 = disp_velo_acce(h, y, k3, m, c, k, f[1])

  return _eval_rk4(h, y, k1, k2, k3. k4, m, c, k, f[1])


def rk4vibration(t, u, v, a, m, c, k, f):
  nmodes = u.shape[0]
  N = u.shape[1]
  for j in range(N):
    dt = t[j+1] - t[j]
    for i in range(nmodes):
      ui, vi, ai = _rk4(dt, [u[i,j], v[i,j], a[i,j]], m[i], c[i], k[i], [f[i,j], f[i,j+1]])
      u[i,j+1] = ui
      v[i,j+1] = vi
      a[i,j+1] = ai

  return u, v, a

