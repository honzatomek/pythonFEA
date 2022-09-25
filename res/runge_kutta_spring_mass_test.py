#!/usr/bin/python
# from: https://www.ritchievink.com/blog/2017/04/13/writing-a-fourth-order-runga-kutta-solver-for-a-vibrations-problem-in-python-part-3/
import numpy as np
import matplotlib.pyplot as plt

km = np.array([ 0.0078,  0.0721,  0.2981,  0.8803,  4.0676, 61.4224])
mm = np.array([1., 1., 1., 1., 1., 1.])
cm = np.array([0.0202, 0.0214, 0.026,  0.0376, 0.1014, 1.2484])
q_0 = np.array([-95.6166, 0., -1.0865, 0., 0.1696, -0.0301])

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
    dt = t[1] - t[0]

    # Returns the acceleration a
    def func(u, V, force):
        return (force - c * V - k * u) / m

    for i in range(t.size - 1):
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

n = 1000
t = np.linspace(0, 1000, n)
force = np.zeros(n)

# for i in range(100, 150):
#       a = np.pi / 50 * (i - 100)
#       force[i] = np.sin(a)

km =  np.array([  0.0078, 0.0721, 0.2981, 0.8803, 4.0676, 61.4224])
mm =  np.array([      1.,     1.,     1.,     1.,     1.,      1.])
cm =  np.array([  0.0202, 0.0214,  0.026, 0.0376, 0.1014,  1.2484])
q_0 = np.array([-95.6166,     0.,-1.0865,     0., 0.1696, -0.0301])

# Parameters of the mass spring system
m =  mm[0]
k =  km[0]
c =  cm[0]
u = q_0[0]

u, v = runge_kutta_vibrations(t, u, 0, m, c, k, force)

print(u)

# Plot the result
fig, ax1 = plt.subplots()
l1 = ax1.plot(t, v, color='b', label="displacement")
ax2 = ax1.twinx()
l2 = ax2.plot(t, force, color='r', label="force")

lines = l1 + l2
plt.legend(lines, [l.get_label() for l in lines])
plt.show()

