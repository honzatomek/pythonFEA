function circles_test08 ( )

%*****************************************************************************80
%
%% circles_test08() tests circles().
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    31 March 2016
%
%  Author:
%
%    Chad Greene
%    Modifications by John Burkardt
%
  fprintf ( 1, '\n' );
  fprintf ( 1, 'circles_test08():\n' );
  fprintf ( 1, '  Circles have corners.\n' );
  fprintf ( 1, '  This script approximates circles as polygons with.\n' );
  fprintf ( 1, '  1000 vertices.  If all those corners are too complex\n' );
  fprintf ( 1, '  for your picture, you can reduce the number of points\n' );
  fprintf ( 1, '  used to make each circle.  Or if 1000 points is not \n' );
  fprintf ( 1, '  high enough resolution, you can increase the number\n' );
  fprintf ( 1, '  of points.  Or if you''d like to draw triangles or squares\n' );
  fprintf ( 1, '  or pentagons, you can significantly reduce the number of \n' );
  fprintf ( 1, '  points.  Let''s try drawing an 8-sided stop sign:\n' );
  fprintf ( 1, '\n' );
  fprintf ( 1, '  cla\n' );
  fprintf ( 1, '  h = circles ( 1, 1, 10, ''vertices'', 8, ''color'', ''red'' ); \n' );
  fprintf ( 1, '  axis equal\n' );

  cla
  h = circles ( 1, 1, 10, 'vertices', 8, 'color', 'red' ); 
  axis equal
%
%  Save a copy of the image as a file.
%
  filename = 'circles_test08.png';
  print ( '-dpng', filename );
  fprintf ( 1, '\n' );
  fprintf ( 1, '  Image saved as "%s"\n', filename );

  return
end
