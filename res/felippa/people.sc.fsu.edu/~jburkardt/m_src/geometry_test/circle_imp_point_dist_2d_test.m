function circle_imp_point_dist_2d_test ( )

%*****************************************************************************80
%
%% circle_imp_point_dist_2d_test() tests circle_imp_point_dist_2d().
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    14 January 2018
%
%  Author:
%
%    John Burkardt
%
  ntest = 5;
  center(1:2,1) = [ 0.0; 0.0 ];
  r = 5.0;
 
  fprintf ( 1, '\n' );
  fprintf ( 1, 'circle_imp_point_dist_2d_test():\n' );
  fprintf ( 1, '  circle_imp_point_dist_2d() checks, by finding the\n' );
  fprintf ( 1, '  distance D from a point (X,Y) to a circle.\n' );

  circle_imp_print_2d ( r, center, '  The circle:' );

  fprintf ( 1, '\n' );
  fprintf ( 1, '       X       Y       D\n' );
  fprintf ( 1, '\n' );

  for i = 1 : 10

    x = - 10.0 + 20.0 * rand ( 1, 1 );
    y = - 10.0 + 20.0 * rand ( 1, 1 );
    d = circle_imp_point_dist_2d ( r, center, [ x; y ] );
    fprintf ( 1, '  %8.4f  %8.4f  %8.4f\n', x, y, d );

  end

  return
end
