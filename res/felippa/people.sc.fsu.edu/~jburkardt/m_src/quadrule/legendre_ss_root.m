function [ x, dp2, p1 ] = legendre_ss_root ( x, n, c )

%*****************************************************************************80
%
%% legendre_ss_root() improves an approximate root of a Legendre polynomial.
%
%  Discussion:
%
%    The Stroud and Secrest reference did not print a specific computer program
%    for the Gauss-Legendre case.  Therefore, this code is based on the
%    printed code for the Gauss-Jacobi case, with ALPHA and BETA set to 0.
%    This means that the LEGENDRE_SS_ROOT and LEGENDRE_SS_RECUR routines,
%    while appropriate for this computation, do not use the standard
%    normalization for the Legendre polynomials in which Pn(1) = 1.
%    The unusual scaling does not, however, affect the location of the
%    roots, which is the primary thing of interest.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    20 June 2015
%
%  Author:
%
%    Original FORTRAN77 version by Arthur Stroud, Don Secrest.
%    MATLAB version by John Burkardt.
%
%  Reference:
%
%    Arthur Stroud, Don Secrest,
%    Gaussian Quadrature Formulas,
%    Prentice Hall, 1966,
%    LC: QA299.4G3S7.
%
%  Input:
%
%    real X, the approximate root, which
%    should be improved on output.
%
%    integer N, the order of the polynomial.
%
%    real C(N), the recursion coefficients.
%
%  Output:
%
%    real X, the approximate root, which
%    should be improved on output.
%
%    real DP2, the value of L'(N)(X).
%
%    real P1, the value of L(N-1)(X).
%
  maxstep = 10;

  for i = 1 : maxstep

    [ p2, dp2, p1 ] = legendre_ss_recur ( x, n, c );

    d = p2 / dp2;
    x = x - d;

    if ( abs ( d ) <= eps * ( abs ( x ) + 1.0 ) )
      return;
    end

  end

  return
end
