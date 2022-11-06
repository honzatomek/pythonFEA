function circles_test09 ( )

%*****************************************************************************80
%
%% circles_test09() tests circles().
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
  fprintf ( 1, 'circles_test09():\n' );
  fprintf ( 1, '  The stop sign in the previous test needs to be rotated.\n' );
  fprintf ( 1, '\n' );
  fprintf ( 1, '  cla\n' );
  fprintf ( 1, '  h = circles ( 1, 1, 10, ''vertices'', 8, ..\n' );
  fprintf ( 1, '    ''color'', ''red'', ''rot'', 45/2 );\n' ); 
  fprintf ( 1, '  text ( 1, 1, ''STOP'', ''fontname'',''helvetica CY'', ...\n' );
  fprintf ( 1, '    ''horizontalalignment'', ''center'', ''fontsize'', 120, ...\n' );
  fprintf ( 1, '    ''color'', ''w'', ''fontweight'', ''bold'' ) \n' );
  fprintf ( 1, '  axis equal\n' );

  cla
  h = circles ( 1, 1, 10, 'vertices', 8, 'color', 'red', 'rot', 45/2 ); 
  text ( 1, 1, 'STOP', 'fontname','helvetica CY', ...
    'horizontalalignment', 'center', 'fontsize', 120, ...
    'color', 'w', 'fontweight', 'bold' ) 
  axis equal
%
%  Save a copy of the image as a file.
%
  filename = 'circles_test09.png';
  print ( '-dpng', filename );
  fprintf ( 1, '\n' );
  fprintf ( 1, '  Image saved as "%s"\n', filename );

  return
end
