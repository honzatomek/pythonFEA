function legendre_2d_exactness ( a, b, n, x, y, w, t )

%*****************************************************************************80
%
%% legendre_2d_exactness(): monomial exactness for the 2D Legendre integral.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license. 
%
%  Modified:
%
%    30 May 2014
%
%  Author:
%
%    John Burkardt
%
%  Input:
%
%    real A(2), the lower limits of integration.
%
%    real B(2), the upper limits of integration.
%
%    integer N, the number of points in the rule.
%
%    real X(N), Y(N), the quadrature points.
%
%    real W(N), the quadrature weights.
%
%    integer T, the maximum total degree.
%    0 <= T.
%
  fprintf ( 1, '\n' );
  fprintf ( 1, '  Quadrature rule for the 2D Legendre integral.\n' );
  fprintf ( 1, '  Number of points in rule is %d\n', n );
  fprintf ( 1, '\n' );
  fprintf ( 1, '   D   I       J          Relative Error\n' );

  for tt = 0 : t

    fprintf ( 1, '  %2d\n', tt );

    for j = 0 : tt

      i = tt - j;

      p(1) = i;
      p(2) = j;

      s = legendre_2d_monomial_integral ( a, b, p );

      v(1:n,1) = x(1:n,1) .^ p(1) .* y(1:n,1) .^ p(2);

      q = w' * v;

      if ( s == 0.0 )
        e = abs ( q );
      else
        e = abs ( q - s ) / abs ( s );
      end

      fprintf ( 1, '  %6d  %6d  %24.16g\n', p(1), p(2), e );

    end
  end

  return
end
