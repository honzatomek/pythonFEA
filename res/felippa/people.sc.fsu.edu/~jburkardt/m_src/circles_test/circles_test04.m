function circles_test04 ( )

%*****************************************************************************80
%
%% circles_test04() tests circles().
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
  fprintf ( 1, 'circles_test04():\n' );
  fprintf ( 1, '  Draw green circles of various locations and sizes.\n' );
  fprintf ( 1, '\n' );
  fprintf ( 1, '  x = 22:27;\n' );
  fprintf ( 1, '  y = [5,15,12,25,3,18];\n' );
  fprintf ( 1, '  r = [3 4 5 5 7 3]; \n' );
  fprintf ( 1, '  cla\n' );
  fprintf ( 1, '  circles ( x, y, r, ''FaceColor'', ''Green'' )\n' );
  fprintf ( 1, '  axis equal\n' );

  x = 22:27;
  y = [5,15,12,25,3,18]; 
  r = [3 4 5 5 7 3]; 

  cla
  circles ( x, y, r, 'FaceColor', 'Green' )
  axis equal
%
%  Save a copy of the image as a file.
%
  filename = 'circles_test04.png';
  print ( '-dpng', filename );
  fprintf ( 1, '\n' );
  fprintf ( 1, '  Image saved as "%s"\n', filename );

  return
end

