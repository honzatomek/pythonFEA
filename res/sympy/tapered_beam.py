#!/usr/bin/python3

from sympy import *


def tapered_beam_K():
    l, x, Em, A, Ai, Aj = symbols('l x Em A Ai Aj')

    print('\nxi =')
    xi = x / l
    pprint(xi)

    print('\nA =')
    A = Ai * (1 - xi) + Aj * xi
    pprint(A)

    print('\nNe =')
    Ne = Matrix([1 - xi, xi]).T
    pprint(Ne)

    print('\nBe =')
    Be = diff(Ne, x)
    pprint(Be)

    print('\nBeT =')
    pprint(Be.T)

    print('\nBeT * Be =')
    pprint(Be.T * Be)

    print('\nKe =')
    Ke = integrate(Em * A * Be.T * Be, (x, 0, l))
    pprint(Ke)

    print('\nsimplified Ke =')
    Kes = simplify(Ke)
    pprint(Kes)

    print('\ncheck for Aj = Ai:')
    Ke1 = simplify(Kes.subs(Aj, Ai))
    pprint(Ke1)

    print('\nsimplified:')
    g = gcd(tuple(Ke1))
    Ke2 = MatMul(g, (Ke1/g), evaluate=False)
    pprint(Ke2)


def tapered_beam_fg():
    q, A, Ai, Aj, ro, g, l, x = symbols('q, A, Ai, Aj, ro, g, l, x')

    print('\nxi =')
    xi = x / l
    pprint(xi)

    print('\nNe =')
    Ne = Matrix([1 - xi, xi]).T
    pprint(Ne)

    print('\nA =')
    A = Ai * (1 - xi) + Aj * xi
    pprint(A)

    print('\nq =')
    q = ro * g * A
    pprint(q)

    print('\nfe =')
    fe = Integral(q * Ne, (x, 0, l))
    pprint(fe)
    fe = simplify(fe.doit())
    pprint(fe)

    print('\ntest for Aj = Ai')
    fes = simplify(fe.subs(Aj, Ai))
    pprint(fes)

    print('\nsimplified:')
    g = gcd(tuple(fes))
    fes = MatMul(g, (fes/g), evaluate=False)
    pprint(fes)


if __name__ == '__main__':
    init_printing(use_unicode=True)
    # tapered_beam_K()
    tapered_beam_fg()
