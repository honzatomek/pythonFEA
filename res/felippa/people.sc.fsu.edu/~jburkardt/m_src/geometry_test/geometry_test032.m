function geometry_test032 ( )

%*****************************************************************************80
%
%% geometry_test032() tests hexagon01_shape_2d().
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    25 January 2019
%
%  Author:
%
%    John Burkardt
%
  fprintf ( 1, '\n' );
  fprintf ( 1, 'geometry_test032():\n' );
  fprintf ( 1, '  hexagon01_shape_2d(): points on a unit hexagon.\n' );
  fprintf ( 1, '\n' );
  fprintf ( 1, '  Angle    X    Y\n' );
  fprintf ( 1, '\n' );

  for i = -10 : 10 : 370
    ang = i;
    p = hexagon01_shape_2d ( ang );
    fprintf ( 1, '  %8f    %12f  %12f\n', ang, p(1:2) );
  end
 
  return
end

