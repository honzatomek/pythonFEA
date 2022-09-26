#!/usr/bin/python
# from: https://www.ritchievink.com/blog/2017/04/13/writing-a-fourth-order-runga-kutta-solver-for-a-vibrations-problem-in-python-part-3/

import numpy as np
import matplotlib.pyplot as plt


def runge_kutta_vibrations(t, u0, v0, m, c, k, force):
    """
    :param t: (list/ array)
    :param u0: (flt)u at t[0]
    :param v0: (flt) v at t[0].
    :param m:(flt) Mass.
    :param c: (flt) Damping.
    :param k: (flt) Spring stiffness.
    :param force: (list/ array) Force acting on the system.
    :return: (tpl) (displacement u, velocity v)
    """

    u = np.zeros(t.shape)
    v = np.zeros(t.shape)
    u[0] = u0
    v[0] = v0

    # Returns the acceleration a
    def func(u, v, force):
        return (force - c * v - k * u) / m

    for i in range(t.size - 1):
        dt = t[i+1] - t[i] # adaptive step size
        # F at time step t / 2
        f_t_05 = (force[i + 1] - force[i]) / 2 + force[i]

        u1 = u[i]
        v1 = v[i]
        a1 = func(u1, v1, force[i])
        u2 = u[i] + v1 * dt / 2
        v2 = v[i] + a1 * dt / 2
        a2 = func(u2, v2, f_t_05)
        u3 = u[i] + v2 * dt / 2
        v3 = v[i] + a2 * dt / 2
        a3 = func(u3, v3, f_t_05)
        u4 = u[i] + v3 * dt
        v4 = v[i] + a3 * dt
        a4 = func(u4, v4, force[i + 1])
        u[i + 1] = u[i] + dt / 6 * (v1 + 2 * v2 + 2 * v3 + v4)
        v[i + 1] = v[i] + dt / 6 * (a1 + 2 * a2 + 2 * a3 + a4)

    return u, v


# modal damping
zeta = 0.08
omega =  np.array([  0.0078, 0.0721, 0.2981, 0.8803, 4.0676,  61.4224])
f = np.sqrt(omega) / (2. * np.pi)
fmax = np.max(f)
print(f'fmax = {fmax}')
km =  np.array([  0.0078, 0.0721, 0.2981, 0.8803, 4.0676,  61.4224])
mm =  np.array([      1.,     1.,     1.,     1.,     1.,       1.])
cm =  np.array([  0.0202, 0.0214,  0.026, 0.0376, 0.1014,   1.2484])
q_0 = np.array([-95.6166,     0.,-1.0865,     0., 0.1696,  -0.0301])

# modal damping
# cm = 2. * zeta * omega
# cm = np.ones(km.shape[0]) * zeta
# print(f'cm = {cm}')
t0 = 0.
tmax = 1000.
# at least 4 points must be present to represent a sine function
n = int((tmax - t0) * (4 * fmax))
t = np.linspace(t0, tmax, n)
force = np.zeros(n)

# for i in range(100, 150):
#       a = np.pi / 50 * (i - 100)
#       force[i] = np.sin(a)

fig, ax1 = plt.subplots()
lines = []

# Parameters of the mass spring system
for i in range(km.shape[0]):
    m =  mm[i]
    k =  km[i]
    c =  cm[i]
    u = q_0[i]

    u, v = runge_kutta_vibrations(t, u, 0, m, c, k, force)

    # print(u[list(range(0,n,100))])
    l = ax1.plot(t, u, label=f"displacement {i+1}")
    lines.append(l)
    print(u)

# Plot the result
# fig, ax1 = plt.subplots()
# l1 = ax1.plot(t, u, color='b', label="displacement")
# ax2 = ax1.twinx()
# l2 = ax2.plot(t, force, color='r', label="force")

# lines = l1 + l2
lns = lines[0]
for i in range(1, len(lines)):
    lns += lines[i]
plt.legend(lns, [l.get_label() for l in lns])
plt.show()

