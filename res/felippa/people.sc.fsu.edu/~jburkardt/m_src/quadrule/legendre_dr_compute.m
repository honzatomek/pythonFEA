function [ x, w ] = legendre_dr_compute ( n )

%*****************************************************************************80
%
%% legendre_dr_compute(): Gauss-Legendre quadrature by Davis-Rabinowitz method.
%
%  Discussion:
%
%    The integral:
%
%      Integral ( -1 <= X <= 1 ) F(X) dX
%
%    The quadrature rule:
%
%      Sum ( 1 <= I <= N ) W(I) * F ( X(I) )
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    10 February 2010
%
%  Author:
%
%    Original FORTRAN77 version by Philip Davis, Philip Rabinowitz.
%    MATLAB version by John Burkardt.
%
%  Reference:
%
%    Philip Davis, Philip Rabinowitz,
%    Methods of Numerical Integration,
%    Second Edition,
%    Dover, 2007,
%    ISBN: 0486453391,
%    LC: QA299.3.D28.
%
%  Input:
%
%    integer N, the order.
%    N must be greater than 0.
%
%  Output:
%
%    real X(N), the abscissas.
%
%    real W(N), the weights.
%
  x = zeros ( n, 1 );
  w = zeros ( n, 1 );

  if ( n < 1 )
    fprintf ( 1, '\n' );
    fprintf ( 1, 'LEGENDRE_DR_COMPUTE - Fatal error!\n' );
    fprintf ( 1, '  Illegal value of N = %d\n', n );
    error ( 'LEGENDRE_DR_COMPUTE - Fatal error!' );
  end

  e1 = n * ( n + 1 );

  m = floor ( ( n + 1 ) / 2 );

  for i = 1 : floor ( ( n + 1 ) / 2 )

    mp1mi = m + 1 - i;

    t = ( 4 * i - 1 ) * pi / ( 4 * n + 2 );

    x0 = cos(t) * ( 1.0 - ( 1.0 - 1.0 / ( n ) ) / ( 8 * n * n ) );

    pkm1 = 1.0;
    pk = x0;

    for k = 2 : n
      pkp1 = 2.0 * x0 * pk - pkm1 - ( x0 * pk - pkm1 ) / k;
      pkm1 = pk;
      pk = pkp1;
    end

    d1 = n * ( pkm1 - x0 * pk );

    dpn = d1 / ( 1.0 - x0 * x0 );

    d2pn = ( 2.0 * x0 * dpn - e1 * pk ) / ( 1.0 - x0 * x0 );

    d3pn = ( 4.0 * x0 * d2pn + ( 2.0 - e1 ) * dpn ) / ( 1.0 - x0 * x0 );

    d4pn = ( 6.0 * x0 * d3pn + ( 6.0 - e1 ) * d2pn ) / ( 1.0 - x0 * x0 );

    u = pk / dpn;
    v = d2pn / dpn;
%
%  Initial approximation H:
%
    h = - u * ( 1.0 + 0.5 * u * ( v + u * ( v * v - d3pn / ( 3.0 * dpn ) ) ) );
%
%  Refine H using one step of Newton's method:
%
    p = pk + h * ( dpn + 0.5 * h * ( d2pn + h / 3.0 ...
      * ( d3pn + 0.25 * h * d4pn ) ) );

    dp = dpn + h * ( d2pn + 0.5 * h * ( d3pn + h * d4pn / 3.0 ) );

    h = h - p / dp;

    xtemp = x0 + h;

    x(mp1mi) = xtemp;

    fx = d1 - h * e1 * ( pk + 0.5 * h * ( dpn + h / 3.0 ...
      * ( d2pn + 0.25 * h * ( d3pn + 0.2 * h * d4pn ) ) ) );

    w(mp1mi) = 2.0 * ( 1.0 - xtemp * xtemp ) / fx / fx;

  end

  if ( mod ( n, 2 ) == 1 )
    x(1) = 0.0;
  end
%
%  Shift the data up.
%
  nmove = floor ( ( n + 1 ) / 2 );
  ncopy = n - nmove;

  for i = 1 : nmove
    iback = n + 1 - i;
    x(iback) = x(iback-ncopy);
    w(iback) = w(iback-ncopy);
  end
%
%  Reflect values for the negative abscissas.
%
  for i = 1 : n - nmove
    x(i) = - x(n+1-i);
    w(i) = w(n+1-i);
  end

  return
end
