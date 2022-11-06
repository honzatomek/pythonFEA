function [ x, w ] = legendre_ss_compute ( n )

%*****************************************************************************80
%
%% legendre_ss_compute(): Gauss-Legendre quadrature by Stroud-Secrest method.
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
%    The integral:
%
%      integral ( -1 <= x <= 1 ) f(x) dx
%
%    The quadrature rule:
%
%      sum ( 1 <= i <= n ) w(i) * f ( x(i) )
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
%    John Burkardt.
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
%    integer N, the order.
%
%  Output:
%
%    real X(N), the abscissas.
%
%    real W(N), the weights.
%
  x = zeros ( n, 1 );
  w = zeros ( n, 1 );
%
%  Set the recursion coefficients.
%
  c = zeros ( n );

  for i = 1 : n
    c(i) = ( ( i - 1 ) * ( i - 1 ) ) / ( ( 2 * i - 1 ) * ( 2 * i - 3 ) );
  end

  cc = 2.0 * prod ( c(2:n) );

  for i = 1 : n

    if ( i == 1 )

      r = 2.78 / ( 4.0 + n * n );

      xtemp = 1.0 - r;

    elseif ( i == 2 )

      r = 1.0 + 0.06 * ( n - 8 ) / n;

      xtemp = xtemp - 4.1 * r * ( 1.0 - xtemp );

    elseif ( i == 3 )

      r = 1.0 + 0.22 * ( n - 8 ) / n;

      xtemp = xtemp - 1.67 * r * ( x(1) - xtemp );

    elseif ( i < n - 1 )

      xtemp = 3.0 * x(i-1) - 3.0 * x(i-2) + x(i-3);

    elseif ( i == n - 1 )

      r = 1.0 / ( 1.0 + 0.639 * ( n - 4 ) / ( 1.0 + 0.71 * ( n - 4 ) ) );

      xtemp = xtemp + r * ( xtemp - x(i-2) ) / 0.766;

    elseif ( i == n )

      r = 1.0 / ( 1.0 + 0.22 * real ( n - 8 ) / n );

      xtemp = xtemp + r * ( xtemp - x(i-2) ) / 1.67;

    end

    [ xtemp, dp2, p1 ] = legendre_ss_root ( xtemp, n, c );

    x(i) = xtemp;
    w(i) = cc / dp2 / p1;

  end
%
%  Reverse the data.
%
  x(1:n) = x(n:-1:1);
  w(1:n) = w(n:-1:1);

  return
end
