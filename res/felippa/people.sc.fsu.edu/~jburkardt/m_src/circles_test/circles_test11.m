function circles_test11 ( )

%*****************************************************************************80
%
%% circles_test11() tests circles().
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
%    John Burkardt
%
  fprintf ( 1, '\n' );
  fprintf ( 1, 'circles_test11():\n' );
  fprintf ( 1, '  Draw a grid of circles, use a color map, and color\n' );
  fprintf ( 1, '  each circle according to sqrt(x^2+y^2).\n' );
  fprintf ( 1, '\n' );
  fprintf ( 1, '  cla\n' );
  fprintf ( 1, '  c = colormap ( ''jet'' );\n' );
  fprintf ( 1, '  [ m, ~ ] = size ( c );\n' );
  fprintf ( 1, '  r = 0.075; \n' );

  fprintf ( 1, '  r = 0.075; \n' );
  fprintf ( 1, '  zmin = 0.0; \n' );
  fprintf ( 1, '  zmax = sqrt ( 8.0 ); \n' );
  fprintf ( 1, '  hold on; \n' );
  fprintf ( 1, '  for i = 0 : 8; \n' );
  fprintf ( 1, '    x = i / 4.0; \n' );
  fprintf ( 1, '    for j = 0 : 8; \n' );
  fprintf ( 1, '      y = j / 4.0; \n' );
  fprintf ( 1, '      z = sqrt ( x * x + y * y );\n' );
  fprintf ( 1, '      indx = fix ( ( m - 1 ) * ( z - zmin ) / ( zmax - zmin ) ) + 1;\n' );
  fprintf ( 1, '      circles ( x, y, r, ''facecolor'', c(indx,1:3) );\n' );
  fprintf ( 1, '    end\n' );
  fprintf ( 1, '  end\n' );
  fprintf ( 1, '  axis equal\n' );
  fprintf ( 1, '  hold off\n' );

  cla
  c = colormap ( 'jet' );
  [ m, ~ ] = size ( c );

  r = 0.075;
  zmin = 0.0;
  zmax = sqrt ( 8.0 );

  hold on
  for i = 0 : 8
    x = i / 4.0;
    for j = 0 : 8
      y = j / 4.0;
      z = sqrt ( x * x + y * y );
      indx = fix ( ( m - 1 ) * ( z - zmin ) / ( zmax - zmin ) ) + 1;
      circles ( x, y, r, 'facecolor', c(indx,1:3) );
    end
  end

  axis equal
  hold off
%
%  Save a copy of the image as a file.
%
  filename = 'circles_test11.png';
  print ( '-dpng', filename );
  fprintf ( 1, '\n' );
  fprintf ( 1, '  Image saved as "%s"\n', filename );

  return
end
