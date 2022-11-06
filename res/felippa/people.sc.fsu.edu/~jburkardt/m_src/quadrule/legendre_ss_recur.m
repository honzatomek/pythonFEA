function [ p2, dp2, p1 ] = legendre_ss_recur ( x, n, c )

%*****************************************************************************80
%
%% legendre_ss_recur() finds the value and derivative of a Legendre polynomial.
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
%    12 October 2005
%
%  Author:
%
%    John Burkardt
%
%  Input:
%
%    real X, the point at which polynomials are evaluated.
%
%    integer N, the order of the polynomial to be computed.
%
%    real C(N), the recursion coefficients.
%
%  Output:
%
%    real P2, the value of P(N)(X).
%
%    real DP2, the value of P'(N)(X).
%
%    real P1, the value of P(N-1)(X).
%
  p1 = 1.0;
  dp1 = 0.0;

  p2 = x;
  dp2 = 1.0;

  for i = 2 : n

    p0 = p1;
    dp0 = dp1;

    p1 = p2;
    dp1 = dp2;

    p2 = x * p1 - c(i) * p0;
    dp2 = x * dp1 + p1 - c(i) * dp0;

  end

  return
end
