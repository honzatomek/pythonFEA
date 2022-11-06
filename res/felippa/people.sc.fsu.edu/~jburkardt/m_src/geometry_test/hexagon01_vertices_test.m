function hexagon01_vertices_test ( )

%*****************************************************************************80
%
%% hexagon01_vertices_test() tests hexagon01_vertices().
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
  fprintf ( 1, 'hexagon01_vertices_test():\n' );
  fprintf ( 1, '  hexagon01_vertices_2d(): the vertices of the unit hexagon.\n' );

  p = hexagon01_vertices ( );

  r8mat_transpose_print ( 2, 6, p, '  Vertices:' );

  return
end
