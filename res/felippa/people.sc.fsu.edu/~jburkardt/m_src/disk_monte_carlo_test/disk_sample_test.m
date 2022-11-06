function disk_sample_test ( center, r )

%*****************************************************************************80
%
%% disk_sample_test() uses disk_sample() to estimate integrals.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    04 July 2018
%
%  Author:
%
%    John Burkardt
%
%  Input:
%
%    real CENTER(2), the center of the disk.
%
%    real R, the radius of the disk.
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
  fprintf ( 1, 'disk_sample_test():\n' );
  fprintf ( 1, '  Use DISK_SAMPLE to estimate integrals in the disk\n' );
  fprintf ( 1, '  with center (%g,%g) and radius %g\n', ...
    center(1), center(2), r );

  fprintf ( 1, '\n' );
  fprintf ( 1, '         N        1              X^2             Y^2        ' );
  fprintf ( 1, '     X^4             X^2Y^2           Y^4             X^6\n' );
  fprintf ( 1, '\n' );

  n = 1;

  while ( n <= 65536 )

    x = disk_sample ( center, r, n );

    fprintf ( 1, '  %8d', n );
    for j = 1 : 7

      e(1:2) = e_test(1:2,j);

      value = monomial_value ( 2, n, e, x );

      result(j) = disk_area ( center, r ) * sum ( value(1:n) ) / n;
      fprintf ( 1, '  %14.6g', result(j) );
    end
    fprintf ( 1, '\n' );

    n = 2 * n;

  end

  if ( ...
    center(1) == 0.0 && ...
    center(2) == 0.0 && ...
    r == 1.0 )
    fprintf ( 1, '\n' );
    fprintf ( 1, '     Exact' );
    for j = 1 : 7
      e(1:2) = e_test(1:2,j);
      result(j) = disk01_monomial_integral ( e );
      fprintf ( 1, '  %14.6g', result(j) );
    end
    fprintf ( 1, '\n' );
  end

  return
end
