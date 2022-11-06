function squaresym_monomial_integral_test ( )

%*****************************************************************************80
%
%% squaresym_monomial_integral_test() tests squaresym_monomial_integral().
%
%  Discussion:
%
%    The test compares exact and estimated integrals over the unit square in 2D
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    19 February 2018
%
%  Author:
%
%    John Burkardt
%
  m = 2;
  n = 4192;
  test_num = 20;

  fprintf ( 1, '\n' );
  fprintf ( 1, 'SQUARESYM_MONOMIAL_INTEGRAL_TEST\n' );
  fprintf ( 1, '  SQUARESYM_MONOMIAL_INTEGRAL reports the integral of a\n' );
  fprintf ( 1, '  monomial over the symmetric unit square.\n' );
  fprintf ( 1, '  Compare exact and estimated integrals\n' );
  fprintf ( 1, '  over the interior of the symmetric unit square in 2D.\n' );
%
%  Get sample points.
%
  x = squaresym_sample ( n );

  fprintf ( 1, '\n' );
  fprintf ( 1, '  Number of sample points used is %d\n', n );
%
%  Randomly choose exponents.
%
  fprintf ( 1, '\n' );
  fprintf ( 1, '  Ex  Ey     MC-Estimate           Exact      Error\n' );
  fprintf ( 1, '\n' );

  for test = 1 : test_num

    e = randi ( [ 0, 7 ], m );

    value = monomial_value ( m, n, e, x );

    result = squaresym_area ( ) * sum ( value(1:n) ) / n;
    exact = squaresym_monomial_integral ( e );
    error = abs ( result - exact );

    fprintf ( 1, '  %2d  %2d  %14.6g  %14.6g  %10.2e\n', ...
      e(1:m), result, exact, error );

  end

  return
end
