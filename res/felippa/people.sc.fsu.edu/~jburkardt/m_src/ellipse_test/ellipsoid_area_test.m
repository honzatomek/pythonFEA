function ellipsoid_area_test ( )

%*****************************************************************************80
%
%% ellipsoid_area_test() tests ellipsoid_area().
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    24 March 2021
%
%  Author:
%
%    John Burkardt
%
  fprintf ( 1, '\n' );
  fprintf ( 1, 'ellipsoid_area_test():\n' );
  fprintf ( 1, '  ellipsoid_area_test() computes the surface area of the ellipsoid\n' );
  fprintf ( 1, '    (x/a)^2+(y/b)^2+(z/c)^2=1\n' );

  fprintf ( 1, '\n' );
  fprintf ( 1, '           A           B           C        Area\n' );
  fprintf ( 1, '\n' );

  a = 1.0;
  b = 0.8;
  c = 0.625;
  area = ellipsoid_area ( a, b, c );
  fprintf ( 1, '  %10.4g  %10.4g  %10.4g  %10.4g\n', a, b, c, area );

  a = 1.0;
  b = 1.0;
  c = 0.5;
  area = ellipsoid_area ( a, b, c );
  fprintf ( 1, '  %10.4g  %10.4g  %10.4g  %10.4g\n', a, b, c, area );

  a = 1.0;
  b = 1.0;
  c = 1.0;
  area = ellipsoid_area ( a, b, c );
  fprintf ( 1, '  %10.4g  %10.4g  %10.4g  %10.4g\n', a, b, c, area );

  a = 2.0;
  b = 1.0;
  c = 0.25;
  area = ellipsoid_area ( a, b, c );
  fprintf ( 1, '  %10.4g  %10.4g  %10.4g  %10.4g\n', a, b, c, area );

  a = 2.0;
  b = 3.0;
  c = 4.0;
  area = ellipsoid_area ( a, b, c );
  fprintf ( 1, '  %10.4g  %10.4g  %10.4g  %10.4g\n', a, b, c, area );

  return
end

