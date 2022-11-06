function hypercube_monte_carlo_test01 ( )

%*****************************************************************************80
%
%% hypercube_monte_carlo_test01() estimates integrals over the unit hypercube in 3D.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    18 January 2014
%
%  Author:
%
%    John Burkardt
%
  m = 3;

  e_test = [ ...
    0, 0, 0; ...
    1, 0, 0; ...
    0, 1, 0; ...
    0, 0, 1; ...
    2, 0, 0; ...
    1, 1, 0; ...
    1, 0, 1; ...
    0, 2, 0; ...
    0, 1, 1; ...
    0, 0, 2 ]';

  fprintf ( 1, '\n' );
  fprintf ( 1, 'hypercube_monte_carlo_test01\n' );
  fprintf ( 1, '  Use HYPERCUBE01_SAMPLE to estimate integrals \n' );
  fprintf ( 1, '  along the interior of the unit hypercube in 3D.\n' );

  fprintf ( 1, '\n' );
  fprintf ( 1, '         N' );
  fprintf ( 1, '        1' );
  fprintf ( 1, '               X' );
  fprintf ( 1, '               Y ' );
  fprintf ( 1, '              Z' );
  fprintf ( 1, '               X^2' );
  fprintf ( 1, '              XY' );
  fprintf ( 1, '             XZ' );
  fprintf ( 1, '              Y^2' );
  fprintf ( 1, '             YZ' );
  fprintf ( 1, '               Z^2\n' );
  fprintf ( 1, '\n' );

  n = 1;

  while ( n <= 65536 )

    x = hypercube01_sample ( m, n );
    fprintf ( 1, '  %8d', n );

    for j = 1 : 10

      e(1:m) = e_test(1:m,j);

      value = monomial_value ( m, n, e, x );

      result = hypercube01_volume ( m ) * sum ( value(1:n) ) / n;

      fprintf ( 1, '  %14.6g', result );

    end

    fprintf ( 1, '\n' );

    n = 2 * n;

  end

  fprintf ( 1, '\n' );
  fprintf ( 1, '     Exact' );

  for j = 1 : 10

    e(1:m) = e_test(1:m,j);

    result = hypercube01_monomial_integral ( m, e );
    fprintf ( 1, '  %14.6g', result );

  end

  fprintf ( 1, '\n' );

  return
end
