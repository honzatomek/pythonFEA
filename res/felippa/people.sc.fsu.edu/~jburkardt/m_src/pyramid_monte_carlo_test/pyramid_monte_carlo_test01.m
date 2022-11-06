function pyramid_monte_carlo_test01 ( )

%*****************************************************************************80
%
%% pyramid_monte_carlo_test01 estimates some integrals.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    14 April 2014
%
%  Author:
%
%    John Burkardt
%
  m = 3;
  test_num = 10;

  e_test = [ ...
    0, 0, 0; ...
    0, 0, 1; ...
    2, 0, 0; ...
    0, 2, 0; ...
    0, 0, 2; ...
    2, 0, 1; ...
    0, 2, 1; ...
    0, 0, 3; ...
    2, 2, 0; ...
    2, 0, 2 ]';

  fprintf ( 1, '\n' );
  fprintf ( 1, 'TEST01\n' );
  fprintf ( 1, '  Use PYRAMID01_SAMPLE to estimate integrals\n' );
  fprintf ( 1, '  over the interior of the unit pyramid in 3D.\n' );

  fprintf ( 1, '\n' );
  fprintf ( 1, '         N' );
  fprintf ( 1, '        1' );
  fprintf ( 1, '               Z' );
  fprintf ( 1, '             X^2' );
  fprintf ( 1, '             Y^2' );
  fprintf ( 1, '             Z^2' );
  fprintf ( 1, '            X^2Z' );
  fprintf ( 1, '            Y^2Z' );
  fprintf ( 1, '             Z^3' );
  fprintf ( 1, '          X^2Y^2' );
  fprintf ( 1, '          X^2Z^2\n' );
  fprintf ( 1, '\n' );

  n = 1;

  while ( n <= 65536 )

    x = pyramid01_sample ( n );

    for j = 1 : test_num

      e(1:m) = e_test(1:m,j);

      value = monomial_value ( m, n, e, x );

      result(j) = pyramid01_volume ( ) * sum ( value(1:n) ) / n;

    end

    fprintf ( 1, '  %8d', n );
    for i = 1 : 10
      fprintf ( 1, '  %14.6g', result(i) );
    end
    fprintf ( 1, '\n' );

    n = 2 * n;

  end

  fprintf ( 1, '\n' );

  for j = 1 : 10

    e(1:m) = e_test(1:m,j);

    result(j) = pyramid01_integral ( e );

  end

  fprintf ( 1, '     Exact');
  for i = 1 : 10
    fprintf ( 1, '  %14.6g', result(i) );
  end
  fprintf ( 1, '\n' );

  return
end
