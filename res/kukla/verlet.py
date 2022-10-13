#!/usr/bin/python3
"""
undamped vibration

solving a + 16 pi u = 2F using Verlet Integration
"""

import numpy as np

if __name__ == '__main__':
  # total time
  T = 1.0
  # time step
  h = 0.05
  # number of time steps
  N = int(T / h)

  # known force over time
  F = np.zeros(N + 2, dtype=float)
  F[1:8] = [25., 50., 75., 100., 75., 50., 25.]

  t = np.linspace(0. - h, T, N + 2)
  a = np.zeros(N + 2, dtype=float) # include init. conditions
  u = np.zeros(N + 2, dtype=float) # include init. conditions

  # TODO: first step as linear
  u[1] = 0.02
  a[1] = 46.91

  # Verlet Integration
  for i in range(2, N+2):
    u[i] = 2. * u[i-1] - u[i-2] + a[i-1] * h ** 2.
    a[i] = 2. * F[i] - u[i] * 16. * np.pi ** 2.

  for i in range(t.size):
    print(f'{t[i]:8.3f} {a[i]:12.4f} {u[i]:12.4f} {F[i]:12.1f}')

