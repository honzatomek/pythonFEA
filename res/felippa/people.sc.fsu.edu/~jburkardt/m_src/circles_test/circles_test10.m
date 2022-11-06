function circles_test10 ( )

%*****************************************************************************80
%
%% circles_test10() tests circles().
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
  fprintf ( 1, 'circles_test10():\n' );
  fprintf ( 1, '  Rotation can be a scalar or a matrix.\n' );
  fprintf ( 1, '  Plot some squares declaring arbitrary rotation\n' );
  fprintf ( 1, '  corresponding to each square\n' );
  fprintf ( 1, '\n' );
  fprintf ( 1, '  cla\n' );
  fprintf ( 1, '  circles ( [1 3 5 7], 2, 1, ''vertices'', 4, ...\n' );
  fprintf ( 1, '    ''rot'', [0 45 35 23.1] );\n' );
  fprintf ( 1, '  axis equal\n' );

  cla
  circles ( [1 3 5 7], 2, 1, 'vertices', 4, 'rot', [0 45 35 23.1] );
  axis equal
%
%  Save a copy of the image as a file.
%
  filename = 'circles_test10.png';
  print ( '-dpng', filename );
  fprintf ( 1, '\n' );
  fprintf ( 1, '  Image saved as "%s"\n', filename );

  return
end
