function circles_test06 ( )

%*****************************************************************************80
%
%% circles_test06() tests circles().
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
  fprintf ( 1, 'circles_test06():\n' );
  fprintf ( 1, '  Draw a grid of circles.\n' );
  fprintf ( 1, '\n' );
  fprintf ( 1, '  cla\n' );
  fprintf ( 1, '  lat = repmat ( (10:-1:1)'', 1, 10 );\n' );
  fprintf ( 1, '  lon = repmat ( 1:10, 10, 1 );\n' );
  fprintf ( 1, '  r = rand ( size ( lat ) ); \n' );
  fprintf ( 1, '  circles ( lon, lat, r, ''edgecolor'', ''b'', ...\n' );
  fprintf ( 1, '    ''facecolor'', [0.7255 0.6353 0.5059] );\n' );
  fprintf ( 1, '  axis equal\n' );

  cla
  lat = repmat ( (10:-1:1)', 1, 10 ); 
  lon = repmat ( 1:10, 10, 1 ); 
  r = rand ( size ( lat ) ); 

  circles ( lon, lat, r, 'edgecolor', 'b', ...
    'facecolor', [0.7255 0.6353 0.5059] );
  axis equal
%
%  Save a copy of the image as a file.
%
  filename = 'circles_test06.png';
  print ( '-dpng', filename );
  fprintf ( 1, '\n' );
  fprintf ( 1, '  Image saved as "%s"\n', filename );

  return
end
