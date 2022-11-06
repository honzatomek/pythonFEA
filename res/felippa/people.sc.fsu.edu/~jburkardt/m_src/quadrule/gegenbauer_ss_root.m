function [ x, dp2, p1 ] = gegenbauer_ss_root ( x, n, alpha, c )

%*****************************************************************************80
%
%% gegenbauer_ss_root() improves an approximate root of a Gegenbauer polynomial.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    02 March 2008
%
%  Author:
%
%    John Burkardt
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
%    real X, the approximate root.
%
%    integer N, the order of the polynomial to be computed.
%
%    real ALPHA, the exponent of (1-X^2) in the quadrature rule.
%
%    real C(N), the recursion coefficients.
%
%  Output:
%
%    real X, the improved approximate root.
%
%    real DP2, the value of J'(N)(X).
%
%    real P1, the value of J(N-1)(X).
%
  maxstep = 10;

  for i = 1 : maxstep

    [ p2, dp2, p1 ] = gegenbauer_ss_recur ( x, n, alpha, c );

    d = p2 / dp2;
    x = x - d;

    if ( abs ( d ) <= eps * ( abs ( x ) + 1.0 ) )
      return
    end

  end

  return
end
