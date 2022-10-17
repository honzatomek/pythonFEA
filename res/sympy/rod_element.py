#!/usr/bin/python3

import unicodedata
from sympy import *

def greek(name, case='small'):
    if case == 'big':
        return unicodedata.lookup('GREEK CAPITAL LETTER {0}'.format(name.upper()))
    else:
        return unicodedata.lookup('GREEK SMALL LETTER {0}'.format(name.upper()))

def extract(A):
    B = simplify(A)
    g = gcd(tuple(B))
    return MatMul(g, (B/g), evaluate=False)


def stiffness(Ne, A, Em, x, l, bounds=(-1, 1)):
    print('\nNe =')
    pprint(extract(simplify(Ne)))
    Be = diff(Ne, x)
    print('\nBe =')
    pprint(extract(simplify(Be)))

    Ke = Integral(Em * A * Be.T * Be, (x, bounds[0], bounds[1]))
    Ke = simplify(Ke.doit())
    Ke = extract(Ke)
    return Ke


def load(Ne, A, ro, g, x, l, bounds=(-1, 1)):
    q = ro * g * A
    fe = Integral(q * Ne, (x, bounds[0], bounds[1]))
    fe = simplify(fe.doit())
    fe = extract(fe)
    return fe


def linear_tapered():
    l, x, Em, A, Ai, Aj, q, ro, g = symbols('l x Em A Ai Aj q ' + greek('rho') + ' g' + greek('xi'))
    xi = x / l
    A = Ai * (1 - xi) + Aj * xi
    q = ro * g * A
    Ne = Matrix([1 - xi, xi]).T
    Ke = stiffness(Ne, A, Em, x, l, bounds=(0, l))
    fe = load(Ne, A, ro, g, x, l, bounds=(0, l))

    print('\nKe =')
    pprint(Ke)
    print('\nfe =')
    pprint(fe)


def linear_tapered_ratio():
    l, x, Em, A, Ai, Aj, q, ro, g, xi, k = symbols('l x Em A Ai Aj q ' + greek('rho') + ' g ' + greek('xi') + ' k')
    xi = x / l
    Aj = k * Ai
    A = Ai * (1 - xi) + Aj * xi
    q = ro * g * A
    Ne = Matrix([1 - xi, xi]).T
    Ke = stiffness(Ne, A, Em, x, l, bounds=(0, l))
    fe = load(Ne, A, ro, g, x, l, bounds=(0, l))

    print('\nKe =')
    pprint(Ke)
    print('\nfe =')
    pprint(fe)


def linear():
    l, x, Em, A, q, ro, g, xi = symbols('l x Em A q ' + greek('rho') + ' g ' + greek('xi'))
    xi = x / l
    q = ro * g * A
    Ne = Matrix([1 - xi, xi]).T
    Ke = stiffness(Ne, A, Em, x, l, bounds=(0, l))
    fe = load(Ne, A, ro, g, x, l, bounds=(0, l))

    print('\nKe =')
    pprint(Ke)
    print('\nfe =')
    pprint(fe)


def quadratic():
    l, x, Em, A, q, ro, g, xi = symbols('l x Em A q ' + greek('rho') + ' g ' + greek('xi'))
    xi = x / l
    q = ro * g * A
    Ne = Matrix([2 * xi ** 2 - 3 * xi + 1, -4 * xi ** 2 + 4 * xi, 2 * xi ** 2 - xi]).T
    Ke = stiffness(Ne, A, Em, x, l, bounds=(0, l))
    fe = load(Ne, A, ro, g, x, l, bounds=(0, l))

    print('\nKe =')
    pprint(Ke)
    print('\nfe =')
    pprint(fe)


def cubic():
    l, x, Em, A, q, ro, g, xi = symbols('l x Em A q ' + greek('rho') + ' g ' + greek('xi'))
    xi = x / l
    q = ro * g * A
    Ne = Matrix([- Rational(9,2) * xi ** 3 + 9 * xi ** 2 - Rational(11, 2) * xi + 1,
                 Rational(27, 2) * xi ** 3 - Rational(45, 2) * xi ** 2 + 9 * xi,
                 - Rational(27, 2) * xi ** 3 + 18 * xi ** 2 - Rational(9, 2) * xi,
                 Rational(9, 2) * xi ** 3 - Rational(9, 2) * xi ** 2 + xi]).T
    Ke = stiffness(Ne, A, Em, x, l, bounds=(0, l))
    fe = load(Ne, A, ro, g, x, l, bounds=(0, l))

    print('\nKe =')
    pprint(Ke)
    print('\nfe =')
    pprint(fe)


def cubic_tapered():
    l, x, Em, A, Ai, Aj, q, ro, g, xi = symbols('l x Em A Ai Aj q ' + greek('rho') + ' g ' + greek('xi'))
    xi = x / l
    A = Ai * (1 - xi) + Aj * xi
    q = ro * g * A
    Ne = Matrix([- Rational(9,2) * xi ** 3 + 9 * xi ** 2 - Rational(11, 2) * xi + 1,
                 Rational(27, 2) * xi ** 3 - Rational(45, 2) * xi ** 2 + 9 * xi,
                 - Rational(27, 2) * xi ** 3 + 18 * xi ** 2 - Rational(9, 2) * xi,
                 Rational(9, 2) * xi ** 3 - Rational(9, 2) * xi ** 2 + xi]).T
    Ke = stiffness(Ne, A, Em, x, l, bounds=(0, l))
    fe = load(Ne, A, ro, g, x, l, bounds=(0, l))

    print('\nKe =')
    pprint(Ke)
    print('\nfe =')
    pprint(fe)


def cubic_tapered_ratio():
    l, x, Em, A, Ai, Aj, q, ro, g, xi, k = symbols('l x Em A Ai Aj q ' + greek('rho') + ' g ' + greek('xi') + ' k')
    xi = x / l
    Aj = k * Ai
    A = Ai * (1 - xi) + Aj * xi
    q = ro * g * A
    Ne = Matrix([- Rational(9,2) * xi ** 3 + 9 * xi ** 2 - Rational(11, 2) * xi + 1,
                 Rational(27, 2) * xi ** 3 - Rational(45, 2) * xi ** 2 + 9 * xi,
                 - Rational(27, 2) * xi ** 3 + 18 * xi ** 2 - Rational(9, 2) * xi,
                 Rational(9, 2) * xi ** 3 - Rational(9, 2) * xi ** 2 + xi]).T
    Ke = stiffness(Ne, A, Em, x, l, bounds=(0, l))
    fe = load(Ne, A, ro, g, x, l, bounds=(0, l))

    print('\nKe =')
    pprint(Ke)
    print('\nfe =')
    pprint(fe)


def main():
    pass


if __name__ == '__main__':
    init_printing(use_unicode=True)
    cubic_tapered()
    # cubic_tapered_ratio()

