function gegenbauer_exactness ( n, x, w, p_max, lambda )

%*****************************************************************************80
%
%% gegenbauer_exactness() investigates exactness of Gegenbauer quadrature.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license. 
%
%  Modified:
%
%    10 January 2016
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
%    real LAMBDA, the exponent term.
%    -1/2 < LAMBDA.
%

%
%  Destroy all row vectors!
%
  x = x(:);
  w = w(:);

  fprintf ( 1, '\n' );
  fprintf ( 1, '  Quadrature rule for Gegenbauer integral.\n' );
  fprintf ( 1, '  Lambda = %g\n', lambda );
  fprintf ( 1, '  Rule of order N = %d\n', n );
  fprintf ( 1, '  Degree          Relative Error\n' );
  fprintf ( 1, '\n' );

  for p = 0 : p_max

    s = gegenbauer_integral ( p, lambda );

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

