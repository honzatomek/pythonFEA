function simplex_integrals_test01 ( )

%*****************************************************************************80
%
%% simplex_integrals_test01() compares exact and estimated integrals in 3D.
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
  n = 4192;
  test_num = 20;

  fprintf ( 1, '\n' );
  fprintf ( 1, 'SIMPLEX_INTEGRALS_TEST01\n' );
  fprintf ( 1, '  Estimate monomial integrals using Monte Carlo\n' );
  fprintf ( 1, '  over the interior of the unit simplex in M dimensions.\n' );
%
%  Get sample points.
%
  x = simplex01_sample ( m, n );

  fprintf ( 1, '\n' );
  fprintf ( 1, '  Number of sample points used is %d\n', n );
%
%  Randomly choose exponents.
%
  fprintf ( 1, '\n' );
  fprintf ( 1, '  We randomly choose the exponents.\n' );
  fprintf ( 1, '\n' );
  fprintf ( 1, '  Ex  Ey  Ez     MC-Estimate      Exact           Error\n' );
  fprintf ( 1, '\n' );

  for test = 1 : test_num

    e = randi ( [ 0, 4 ], m );

    value = monomial_value ( m, n, e, x );

    result = simplex01_volume ( m ) * sum ( value(1:n) ) / n;
    exact = simplex01_monomial_integral ( m, e );
    error = abs ( result - exact );

    fprintf ( 1, '  %2d  %2d  %2d  %14.6g  %14.6g  %10.2e\n', ...
      e(1:m), result, exact, error );

  end

  return
end
