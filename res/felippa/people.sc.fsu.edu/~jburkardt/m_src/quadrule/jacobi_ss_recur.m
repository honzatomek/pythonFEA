function [ p2, dp2, p1 ] = jacobi_ss_recur ( x, n, alpha, beta, b, c )

%*****************************************************************************80
%
%% jacobi_ss_recur() finds the value and derivative of a Jacobi polynomial.
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
%    real X, the point at which polynomials are evaluated.
%
%    integer N, the order of the polynomial to be computed.
%
%    real ALPHA, BETA, the exponents of (1-X) and
%    (1+X) in the quadrature rule.
%
%    real B(N), C(N), the recursion coefficients.
%
%  Output:
%
%    real P2, the value of J(N)(X).
%
%    real DP2, the value of J'(N)(X).
%
%    real P1, the value of J(N-1)(X).
%
  p1 = 1.0;
  dp1 = 0.0;

  p2 = x + ( alpha - beta ) / ( alpha + beta + 2.0 );
  dp2 = 1.0;

  for i = 2 : n

    p0 = p1;
    dp0 = dp1;

    p1 = p2;
    dp1 = dp2;

    p2 = ( x - b(i) ) * p1 - c(i) * p0;
    dp2 = ( x - b(i) ) * dp1 + p1 - c(i) * dp0;

  end

  return
end
