#!/usr/bin/python3

from math import atan, cos
from math import pi


# for rhs = F cos(wt)
def sdof_ss(k, c, m, F, w):
  wn = (k / m) ** 0.5
  xstatic = F / k
  z = damping_ratio(c, m, wn)
  return xstatic * amplitude_ratio(w, wn, z), phase_angle(w, wn, z)


def natural_frequency(k, m):
  """
  natural frequency
  In:
    k    - stiffness
    m    - mass
  Out:
    wn   - natural frequency (rad/s)
  """
  return (k / m) ** 0.5


def rads2Hz(w):
  """
  convert natural frequency to frequency
  In:
    w    - natural frequency
  Out:
    f    - frequency in Hz
  """
  return w / (2. * pi)


def Hz2rads(f):
  """
  convert frequency to natural frequency
  In:
    f    - frequency in Hz
  Out:
    w    - natural frequency
  """
  return 2. * pi * f


def period2Hz(T):
  """
  convert period to frequency
  In:
    T    - amplitude period
  Out:
    f    - frequency in Hz
  """
  return 1. / f


def Hz2period(f):
  """
  convert frequency to period
  In:
    f    - frequency in Hz
  Out:
    T    - amplitude period
  """
  return 1. / T


def damping_ratio(c, m, wn):
  """
  damping ratio
  In:
    c    - damping value
    m    - mass
    wn   - natural frequency
  Out:
    zeta - damping ratio
  """
  return c / (2. * m * wn)


def amplitude_ratio(w, wn, z):
  """
  Amplitude ratio transfer function
  In:
    w    - excitation frequency
    wn   - natural frequency
    z    - damping ratio
  Out:
    H(w) - amplitude ratio transfer function
  """
  return 1. / (((1. - (w / wn) ** 2.) ** 2. + (2. * z * w / wn) ** 2) ** 0.5)


def phase_angle(w, wn, z):
  """
  Phase angle transfer function
  In:
    w    - excitation frequency
    wn   - natural frequency
    z    - damping ratio
  Out:
    H(w) - phase angle transfer function
  """
  return  atan((2. * z * w / wn) / (1. - (w / wn) ** 2.))


def test1():
  # structure
  m = 1. # t
  c = 0.002
  f = 2.5 # Hz
  k = m * (2. * pi * f) ** 2. # N/mm

  fl = 3. # Hz
  wl = 2. * pi * fl # rad/s

  # load cos
  Fa = 10000. # N
  A, phiA = sdof_ss(k, c, m, Fa, wl)

  # load sin
  Fb = 500. # N
  B, phiB = sdof_ss(k, c, m, Fb, wl)

  T = 1. / fl
  N = 50
  dt = T / N
  t = 0.

  for i in range(N + 1):
    print(f"t = {t:8.3f} s, x(t) = {A * cos(wl * t + phiA) + B * sin(wl * t + phiB):8.3f} mm")
    t += dt


def test2():
  # structure
  m = 1. # t
  c = 0.002
  f = 2.5 # Hz
  k = m * (2. * pi * f) ** 2. # N/mm

  fl = 3. # Hz
  wl = 2. * pi * fl # rad/s

  z = damping_ratio(c, m. wn)
  Hw = amplitude_ratio(w, wn, z)
  phi = phase_angle(w, wn, z)

  # load cos
  Fa = 10000. # N

  # load sin
  Fb = 500. # N

  T = 1. / fl
  N = 50
  dt = T / N
  t = 0.

  for i in range(N + 1):
    print(f"t = {t:8.3f} s, x(t) = {Fa / k * Hw * cos(wl * t + phi) + Fb / sin(wl * t * phi):8.3f} mm")
    t += dt


def test_2orders():
  # structure
  m = 1. # t
  c = 0.002
  f = 2.5 # Hz
  k = m * (2. * pi * f) ** 2. # N/mm

  fl = 3. # Hz
  wl = 2. * pi * fl # rad/s

  # load 1. order
  F1 = 10000. # N
  A1, phi1 = sdof_ss(k, c, m, F1, wl)

  # load 2. order
  F2 = 50000. # N
  A2, phi2 = sdof_ss(k, c, m, F2, wl * 2.)

  h = 2
  T = 1. / fl
  N = 25 * h
  dt = T / N
  t = 0.

  for i in range(N + 1):
    print(f"t = {t:8.3f} s, x(t) = {A1 * cos(wl * t + phi1) + A2 * cos(2 * wl * t + phi2):8.3f} mm")
    t += dt


def test_phase_shift():
  # structure
  m = 1. # t
  c = 0.002
  f = 2.5 # Hz
  k = m * (2. * pi * f) ** 2. # N/mm

  fl = 3. # Hz
  wl = 2. * pi * fl # rad/s

  # load 1
  F1 = 10000. # N
  phiF1 = pi / 2. # rad
  A1, phi1 = sdof_ss(k, c, m, F1, wl)

  # load 2
  F2 = 5000. # N
  phiF2 = pi / 3. # rad
  A2, phi2 = sdof_ss(k, c, m, F2, wl)

  h = 2
  T = 1. / fl
  N = 25 * h
  dt = T / N
  t = 0.

  for i in range(N + 1):
    print(f"t = {t:8.3f} s, x(t) = {A1 * cos(wl * t + phi1 + phiF1) + A2 * cos(2 * wl * t + phi2 + phiF2):8.3f} mm")
    t += dt


if __name__ == '__main__':
  # test_2orders()
  test_phase_shift()

