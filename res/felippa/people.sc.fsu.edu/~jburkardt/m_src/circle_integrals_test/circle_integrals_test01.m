function circle_integrals_test01 ( )

%*****************************************************************************80
%
%% circle_integrals_test01() tests circle01_sample().
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    12 January 2014
%
%  Author:
%
%    John Burkardt
%
  m = 2;
  n = 4192;
  test_num = 20;

  fprintf ( 1, '\n' );
  fprintf ( 1, 'circle_integrals_test01()\n' );
  fprintf ( 1, '  Use CIRCLE01_SAMPLE to compare exact and\n' );
  fprintf ( 1, '  estimated integrals along the circumference \n' );
  fprintf ( 1, '  of the unit circle in 2D.\n' );
%
%  Get sample points.
%
  x = circle01_sample ( n );

  fprintf ( 1, '\n' );
  fprintf ( 1, '  Number of sample points used is %d\n', n );
%
%  Randomly choose X, Y exponents.
%
  fprintf ( 1, '\n' );
  fprintf ( 1, '  If any exponent is odd, the integral is zero.\n' );
  fprintf ( 1, '  We restrict this test to randomly chosen even exponents.\n' );
  fprintf ( 1, '\n' );
  fprintf ( 1, '  Ex  Ey     MC-Estimate           Exact      Error\n' );
  fprintf ( 1, '\n' );

  for test = 1 : test_num

    e = 2 * randi ( [ 0, 5 ], m );

    value = monomial_value ( m, n, e, x );

    result = circle01_length ( ) * sum ( value(1:n) ) / n;
    exact = circle01_monomial_integral ( e );
    error = abs ( result - exact );

    fprintf ( 1, '  %2d  %2d  %14.6g  %14.6g  %10.2e\n', ...
      e(1:m), result, exact, error );

  end

  return
end
