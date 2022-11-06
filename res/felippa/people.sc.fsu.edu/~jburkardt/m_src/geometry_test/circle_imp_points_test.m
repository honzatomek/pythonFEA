function circle_imp_points_test ( )

%*****************************************************************************80
%
%% circle_imp_points_test() tests circle_imp_points().
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    04 December 2010
%
%  Author:
%
%    John Burkardt
%
  center(1:2,1) =  [ 5.0; -2.0 ];
  r = 2.0;

  fprintf ( 1, '\n' );
  fprintf ( 1, 'circle_imp_points_test():\n' );
  fprintf ( 1, '  circle_imp_points() gets points on a circle;\n' );
  fprintf ( 1, '  polygon_area() finds the area of a polygon.\n' );

  circle_imp_print_2d ( r, center, '  The implicit circle:' );

  fprintf ( 1, '\n' );
  fprintf ( 1, '  The area = %f\n', pi * r * r );

  n = 8;

  p = circle_imp_points ( r, center, n );

  r8mat_print ( 2, n, p, '  Sample results:' );

  fprintf ( 1, '\n' );
  fprintf ( 1, '  For any N, the sampled points define a polygon\n' );
  fprintf ( 1, '  whose area approximates the circle area.\n' );
  fprintf ( 1, '\n' );
  fprintf ( 1, '  N      Area\n' );
  fprintf ( 1, '\n' );

  for n = 3 : 24

    p = circle_imp_points ( r, center, n );
    result = polygon_area ( n, p );
    fprintf ( 1, '  %6d  %12f\n', n, result );

  end

  return
end
