function hypercube_integrals_test02 ( )

%*****************************************************************************80
%
%% hypercube_integrals_test02(): estimate integrals over the unit hypercube in 6D
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
  m = 6;
  n = 4192;
  test_num = 20;

  fprintf ( 1, '\n' );
  fprintf ( 1, 'hypercube_integrals_test02\n' );
  fprintf ( 1, '  Compare exact and estimated integrals\n' );
  fprintf ( 1, '  over the interior of the unit hypercube in 6D.\n' );
%
%  Get sample points.
%
  x = hypercube01_sample ( m, n );

  fprintf ( 1, '\n' );
  fprintf ( 1, '  Number of sample points used is %d\n', n );
%
%  Randomly choose exponents.
%
  fprintf ( 1, '\n' );
  fprintf ( 1, '  E1  E2  E3  E4  E5  E6     MC-Estimate           Exact      Error\n' );
  fprintf ( 1, '\n' );

  for test = 1 : test_num

    e = randi ( [ 0, 3 ], m );
 
    value = monomial_value ( m, n, e, x );

    result = hypercube01_volume ( m ) * sum ( value(1:n) ) / n;
    exact = hypercube01_monomial_integral ( m, e );
    error = abs ( result - exact );

    fprintf ( 1, '  %2d  %2d  %2d  %2d  %2d  %2d  %14.6g  %14.6g  %10.2e\n', ...
      e(1:m), result, exact, error );

  end

  return
end
