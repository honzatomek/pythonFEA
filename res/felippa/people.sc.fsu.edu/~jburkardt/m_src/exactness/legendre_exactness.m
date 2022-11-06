function legendre_exactness ( n, x, w, p_max )

%*****************************************************************************80
%
%% legendre_exactness() investigates exactness of Legendre quadrature.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license. 
%
%  Modified:
%
%    16 May 2014
%
%  Author:
%
%    John Burkardt
%
%  Input:
%
%    integer N, the number of points in the rule.
%
%    real X(N), the quadrature points.
%
%    real W(N), the quadrature weights.
%
%    integer P_MAX, the maximum exponent.
%    0 <= P_MAX.
%

%
%  Destroy all row vectors!
%
  x = x(:);
  w = w(:);

  fprintf ( 1, '\n' );
  fprintf ( 1, '  Quadrature rule for Legendre integral.\n' );
  fprintf ( 1, '  Rule of order N = %d\n', n );
  fprintf ( 1, '  Degree          Relative Error\n' );
  fprintf ( 1, '\n' );

  for p = 0 : p_max

    s = legendre_integral ( p );

    v(1:n,1) = x(1:n) .^ p;

    q = w' * v;

    if ( s == 0.0 )
      e = abs ( q - s );
    else
      e = abs ( ( q - s ) / s );
    end

    fprintf ( 1, '  %6d  %24.16f\n', p, e );

  end

  return
end

