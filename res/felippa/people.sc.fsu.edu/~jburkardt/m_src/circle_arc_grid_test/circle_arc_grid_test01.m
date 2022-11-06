function circle_arc_grid_test01 ( )

%*****************************************************************************80
%
%% circle_arc_grid_test01() tests circle_arc_grid().
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license. 
%
%  Modified:
%
%    29 October 2022
%
%  Author:
%
%    John Burkardt
%
  fprintf ( 1, '\n' );
  fprintf ( 1, 'circle_arc_grid_test01():\n' );
  fprintf ( 1, '  Compute points on a 90 degree arc.\n' );

  r = 2.0;
  c(1) = 5.0;
  c(2) = 5.0;
  a(1) = 0.0;
  a(2) = 90.0;
  n = 10;
%
%  Echo the input.
%
  fprintf ( 1, '\n' );
  fprintf ( 1, '  Radius =           %f\n', r );
  fprintf ( 1, '  Center =           %f  %f\n', c(1), c(2) );
  fprintf ( 1, '  Angle 1 =          %f\n', a(1) );
  fprintf ( 1, '  Angle 2 =          %f\n', a(2) );
  fprintf ( 1, '  Number of points = %d\n', n );
%
%  Compute the data.
%
  xy = circle_arc_grid ( r, c, a, n );
%
%  Print a little of the data.
%
  fprintf ( 1, '\n' );
  fprintf ( 1, '  A few of the points:\n' );
  disp ( xy(1:5,1:2) );
%
%  Write the data.
%
  filename = 'arc.txt';
  save ( filename, 'xy', '-ascii' );
  fprintf ( 1, '  Data saved as "%s"\n', filename );

  return
end
