function circles_test02 ( )

%*****************************************************************************80
%
%% circles_test02() tests circles().
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
  fprintf ( 1, 'circles_test02():\n' );
  fprintf ( 1, '  Draw 10 circles at x = 1:10, y = 5, of radius 2.\n' );
  fprintf ( 1, '  They will automatically be of varying colors.\n' );
  fprintf ( 1, '\n' );
  fprintf ( 1, '  cla\n' );
  fprintf ( 1, '  circles ( 1:10, 5, 2 )\n' );
  fprintf ( 1, '  axis equal\n' );

  cla
  circles ( 1:10, 5, 2 )
  axis equal
%
%  Save a copy of the image as a file.
%
  filename = 'circles_test02.png';
  print ( '-dpng', filename );
  fprintf ( 1, '\n' );
  fprintf ( 1, '  Image saved as "%s"\n', filename );

  return
end

