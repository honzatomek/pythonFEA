function [ x, dp2, p1 ] = laguerre_ss_root ( x, n, b, c )

%*****************************************************************************80
%
%% laguerre_ss_root() improves an approximate root of a Laguerre polynomial.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    20 February 2008
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
%    real B(N), C(N), the recursion coefficients.
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

    [ p2, dp2, p1 ] = laguerre_ss_recur ( x, n, b, c );

    d = p2 / dp2;
    x = x - d;

    if ( abs ( d ) <= eps * ( abs ( x ) + 1.0 ) )
      return;
    end

  end

  return
end
