function circles_test03 ( )

%*****************************************************************************80
%
%% circles_test03() tests circles().
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
  fprintf ( 1, 'circles_test03():\n' );
  fprintf ( 1, '  Draw 5 circles at x = 5, y = 15, of radiuses 1:5\n' );
  fprintf ( 1, '  specifying FaceColor None, resulting in hollow or open circles.\n' );
  fprintf ( 1, '\n' );
  fprintf ( 1, '  cla\n' );
  fprintf ( 1, '  circles ( 3, 15, 1:5, ''FaceColor'', ''None'' )\n' );
  fprintf ( 1, '  axis equal\n' );

  cla
  circles ( 5, 15, 1:5, 'FaceColor', 'None' )
  axis equal
%
%  Save a copy of the image as a file.
%
  filename = 'circles_test03.png';
  print ( '-dpng', filename );
  fprintf ( 1, '\n' );
  fprintf ( 1, '  Image saved as "%s"\n', filename );

  return
end

