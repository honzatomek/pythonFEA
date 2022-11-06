function circles_test01 ( )

%*****************************************************************************80
%
%% circles_test01() tests circles().
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
  fprintf ( 1, 'circles_test01():\n' );
  fprintf ( 1, '  Draw one circle.\n' );
  fprintf ( 1, '  Here we draw one circle of radius 3, centered at (5,10).\n' );
  fprintf ( 1, '  Matlab doesn''t set axes equal by default, so circles may not\n' );
  fprintf ( 1, '  look like circles until you declare |axis equal|.\n' );
  fprintf ( 1, '\n' );
  fprintf ( 1, '  cla\n' );
  fprintf ( 1, '  circles ( 5, 10, 3 )\n' );
  fprintf ( 1, '  axis equal\n' );

  cla
  circles ( 5, 10, 3 )
  axis equal
%
%  Save a copy of the image as a file.
%
  filename = 'circles_test01.png';
  print ( '-dpng', filename );
  fprintf ( 1, '\n' );
  fprintf ( 1, '  Image saved as "%s"\n', filename );

  return
end
 
