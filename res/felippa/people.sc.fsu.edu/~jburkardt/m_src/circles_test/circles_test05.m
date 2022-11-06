function circles_test05 ( )

%*****************************************************************************80
%
%% circles_test05() tests circles().
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
  fprintf ( 1, 'circles_test05():\n' );
  fprintf ( 1, '  Draw a circle at x = 5, y = 10, of radius 3.\n' );
  fprintf ( 1, '  Specify the edge color and linewidth.\n' );
  fprintf ( 1, '\n' );
  fprintf ( 1, '  cla\n' );
  fprintf ( 1, '  circles ( 5, 10, 3, ''edgecolor'', [.5 .2 .9], ...\n' );
  fprintf ( 1, '    ''linewidth'', 4 )\n' );
  fprintf ( 1, '  axis equal\n' );

  cla
  circles ( 5, 10, 3, 'edgecolor', [.5 .2 .9], 'linewidth', 4 )
  axis equal
%
%  Save a copy of the image as a file.
%
  filename = 'circles_test05.png';
  print ( '-dpng', filename );
  fprintf ( 1, '\n' );
  fprintf ( 1, '  Image saved as "%s"\n', filename );

  return
end

