#!/usr/bin/python3

import sympy
from sympy import init_printing, pprint
from sympy import symbols
from sympy import expand, factor
from sympy import sin, exp, cos
from sympy import diff, integrate, limit
from sympy import oo
from sympy import solve
from sympy import Function, Eq, dsolve
from sympy import Matrix
from sympy import Integral, Derivative, Limit
from sympy import latex, pi
from sympy import simplify


def test_sqrt():
    print(sympy.sqrt(8))


def test_symbols():
    x, y = symbols('x y')
    expr = x + 2*y
    pprint(expr)
    pprint(expr + 1)
    pprint(expr - x)
    pprint(x*expr)
    expanded_expr = expand(x*expr)
    pprint(expanded_expr)
    factored_expr = factor(expanded_expr)
    pprint(factored_expr)


def derive_integrate():
    x = symbols('x')
    expr = sin(x) * exp(x)
    print('\nSolve:')
    pprint(Derivative(expr, x))

    derivation = diff(expr, x)
    print('Solution:')
    pprint(derivation)

    print('\nSolve:')
    pprint(Integral(derivation, x))

    integral = integrate(derivation, x)
    print('Solution:')
    pprint(integral)


def infinite_integral():
    x = symbols('x')
    expr = sin(x ** 2)
    print('\nSolve:')
    pprint(Integral(expr, (x, -oo, oo)))

    integral = integrate(expr, (x, -oo, oo))
    print('Solution:')
    pprint(integral)


def find_lim():
    x = symbols('x')
    expr = sin(x) / x
    print('\nSolve:')
    pprint(Limit(expr, x, 0))

    lim = limit(expr, x, 0)
    print('Solution:')
    pprint(lim)


def solve_equation():
    x = symbols('x')
    expr = x ** 2 - 2
    print('\nSolve:')
    pprint(Eq(expr, x))

    sol = solve(expr, x)
    print('Solution:')
    pprint(sol)


def solve_differential_equation():
    x, t = symbols('x t')
    x = Function('x')
    eq = Eq(x(t).diff(t, t) - x(t), exp(t))
    print('\nSolve:')
    pprint(eq)

    sol = dsolve(eq, x(t))
    print('Solution:')
    pprint(sol)


def find_eigvals():
    m = Matrix([[1, 2], [2, 2]])
    print('\nFind eigenvalues of matrix:')
    pprint(m)

    print('Solution:')
    eigs = m.eigenvals()
    pprint(eigs)


def print_latex():
    x = symbols('x')
    eq = Integral(cos(x) ** 2, (x, 0, pi))
    print('\nPrint equation as LaTeX code:')
    pprint(eq)
    print(latex(eq))


def test_equality():
    x = symbols('x')
    a = (x + 1) ** 2
    b = x ** 2 + 2 * x + 1
    eq = Eq(a, b)
    print('\nTest equality:')
    pprint(eq)

    sol = simplify(a - b)
    print('Solution:')
    pprint(sol == 0)
    print('Result:')
    pprint(sol)

    c = x ** 2 - 2 * x + 1
    eq = Eq(a, c)
    print('\nTest equality:')
    pprint(eq)

    sol = simplify(a - c)
    print('Solution:')
    pprint(sol == 0)
    print('Result:')
    pprint(sol)


def test_equality2():
    x = symbols('x')
    a = cos(x) ** 2 - sin(x) ** 2
    b = cos(2 * x)
    eq = Eq(a, b)
    print('\nTest equality:')
    pprint(eq)

    sol = simplify(a - b)
    print('Solution:')
    pprint(sol == 0)
    print('Result:')
    pprint(sol)

    eq = Eq(a, b)
    print('\nTest equality by equals():')
    pprint(eq)

    print('Solution:')
    pprint(a.equals(b))


def substitute_variable():
    x, y = symbols('x y')
    expr = cos(x) + 1
    print('\nSubstitute x for y in:')
    pprint(expr)

    print('Result:')
    pprint(expr.subs(x, y))

    print('\nSubstitute x for 0 in:')
    pprint(expr)

    print('Result:')
    pprint(expr.subs(x, 0))


def chaining_variable_substitution():
    x, y = symbols('x y')
    expr = x ** y
    print('\nSubstitute y for x ** y in:')
    pprint(expr)

    expr = expr.subs(y, x ** y)
    print('Result:')
    pprint(expr)

    print('\nSubstitute y for x ** y in:')
    pprint(expr)

    expr = expr.subs(y, x ** y)
    print('Result:')
    pprint(expr)

    print('\nSubstitute y for x ** x in:')
    pprint(expr)

    expr = expr.subs(y, x ** x)
    print('Result:')
    pprint(expr)


def substitute_part_of_expression():
    x = symbols('x')
    expr = sin(2 * x) + cos(2 * x)
    print('\nSubstitute sin(2 * x) for 2 * sin(x) * cos(x) in:')
    pprint(expr)

    expr = expr.subs(sin(2 * x), 2 * sin(x) * cos(x))
    print('Result:')
    pprint(expr)


def multiple_substitutions():
    x, y = symbols('x y')
    expr = x ** 4 - 4 * x ** 3 + 4 * x ** 2 - 2 * x + 3
    print('\nSubstitute x with even exponent for y in:')
    pprint(expr)

    replacements = [(x ** i, y ** i) for i in range(5) if i % 2 == 0]
    res = expr.subs(replacements)
    print('Result:')
    pprint(res)


def test():
    x, l = symbols('x l')
    xi = symbols('xi')
    N = Matrix([1 - xi, xi]).subs(xi, x / l)
    pprint(N)
    B = N.diff(x)
    pprint(factor(B))


def main():
    # test_sqrt()
    # test_symbols()
    # derive_integrate()
    # infinite_integral()
    # find_lim()
    # solve_equation()
    # solve_differential_equation()
    # find_eigvals()
    # print_latex()
    # test_equality()
    # test_equality2()
    # substitute_variable()
    # chaining_variable_substitution()
    # substitute_part_of_expression()
    # multiple_substitutions()
    test()


if __name__ == '__main__':
    init_printing(use_unicode=True)
    main()
