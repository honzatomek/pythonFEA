function simplex_monte_carlo_test01 ( )

%*****************************************************************************80
%
%% simplex_monte_carlo_test01() estimates integrals in 3D.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    15 January 2014
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
  fprintf ( 1, 'TEST01\n' );
  fprintf ( 1, '  SIMPLEX_UNIT_SAMPLE computes a Monte Carlo estimate of an\n' );
  fprintf ( 1, '  integral over the interior of the unit simplex in 3D.\n' );

  fprintf ( 1, '\n' );
  fprintf ( 1, '         N        1               X               Y ' );
  fprintf ( 1, '              Z               X^2              XY             XZ' );
  fprintf ( 1, '              Y^2             YZ               Z^2\n' );
  fprintf ( 1, '\n' );

  n = 1;

  while ( n <= 65536 )

    x = simplex_unit_sample ( m, n );

    fprintf ( 1, '  %8d', n );

    for j = 1 : 10

      e(1:m) = e_test(1:m,j);

      value = monomial_value ( m, n, e, x );

      result = simplex_unit_volume ( m ) * sum ( value(1:n) ) / n;
      fprintf ( 1, '  %14.6g', result );

    end

    fprintf ( 1, '\n' );

    n = 2 * n;

  end

  fprintf ( 1, '\n' );
  fprintf ( 1, '     Exact' );
  for j = 1 : 10

    e(1:m) = e_test(1:m,j);

    result = simplex_unit_monomial_integral ( m, e );
    fprintf ( 1, '  %14.6g', result );

  end

  fprintf ( 1, '\n' );

  return
end