function annulus_sample_test ( center, r1, r2 )

%*****************************************************************************80
%
%% annulus_sample_test() uses annulus_sample() to estimate integrals.
%
%  Discussion:
%
%    If CENTER=(0,0) and R1 = 0 and R2 = 1, then we can compare exact values.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    20 March 2021
%
%  Author:
%
%    John Burkardt
%
  e_test = [ ...
    0, 0; ...
    2, 0; ...
    0, 2; ...
    4, 0; ...
    2, 2; ...
    0, 4; ...
    6, 0 ]';

  fprintf ( 1, '\n' );
  fprintf ( 1, 'ANNULUS_SAMPLE_TEST\n' );
  fprintf ( 1, '  ANNULUS_SAMPLE can sample an annulus uniformly.\n' );
  fprintf ( 1, '  Use it to estimate integrals in the annulus\n' );
  fprintf ( 1, '  centered at (%g,%g) with R1 = %g, R2 = %g\n', ...
    center(1), center(2), r1, r2 );

  fprintf ( 1, '\n' );
  fprintf ( 1, '         N        1              X^2             Y^2        ' );
  fprintf ( 1, '     X^4             X^2Y^2           Y^4             X^6\n' );
  fprintf ( 1, '\n' );

  n = 1;

  while ( n <= 65536 )

    x = annulus_sample ( center, r1, r2, n );

    fprintf ( 1, '  %8d', n );
    for j = 1 : 7
      e(1:2) = e_test(1:2,j);
      value = monomial_value ( 2, n, e, x );
      result = annulus_area ( center, r1, r2 ) * sum ( value(1:n) ) / n;
      fprintf ( 1, '  %14.6g', result );
    end
    fprintf ( 1, '\n' );

    n = 2 * n;

  end

  if ( ...
    center(1) == 0.0 && ...
    center(2) == 0.0 && ...
    r1 == 0.0 && ...
    r2 == 1.0 )
    fprintf ( 1, '\n' );
    fprintf ( 1, '     Exact' );
    for j = 1 : 7

      e(1:2) = e_test(1:2,j);

      result = disk01_monomial_integral ( e );
      fprintf ( 1, '  %14.6g', result );
    end
    fprintf ( 1, '\n' );

  end

  return
end
