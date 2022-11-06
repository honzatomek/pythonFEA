function hypersphere_integrals_test01 ( )

%*****************************************************************************80
%
%% hypersphere_integrals_test01() compares exact and estimated integrals in 3D.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    07 January 2014
%
%  Author:
%
%    John Burkardt
%
  m = 3;
  n = 4192;
  test_num = 20;

  fprintf ( 1, '\n' );
  fprintf ( 1, 'hypersphere_integrals_test01\n' );
  fprintf ( 1, '  Estimate monomial integrals using Monte Carlo\n' );
  fprintf ( 1, '  over the surface of the unit hypersphere in 3D.\n' );
%
%  Get sample points.
%
  x = hypersphere01_sample ( m, n );

  fprintf ( 1, '\n' );
  fprintf ( 1, '  Number of sample points used is %d\n', n );
%
%  Randomly choose X,Y,Z exponents between (0,0,0) and (8,8,8).
%
  fprintf ( 1, '\n' );
  fprintf ( 1, '  If any exponent is odd, the integral is zero.\n' );
  fprintf ( 1, '  We will restrict this test to randomly chosen even exponents.\n' );
  fprintf ( 1, '\n' );
  fprintf ( 1, '  Ex  Ey  Ez     MC-Estimate           Exact      Error\n' );
  fprintf ( 1, '\n' );

  for test = 1 : test_num

    e = 2 * randi ( [ 0, 4 ], m );

    value = monomial_value ( m, n, e, x );

    result = hypersphere01_area ( m ) * sum ( value(1:n) ) / n;
    exact = hypersphere01_monomial_integral ( m, e );
    error = abs ( result - exact );

    fprintf ( 1, '  %2d  %2d  %2d  %14.6g  %14.6g  %10.2e\n', ...
      e(1:m), result, exact, error );

  end

  return
end
