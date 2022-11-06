function tetrahedron_lattice_layer_point_next_test ( )

%*****************************************************************************80
%
%% tetrahedron_lattice_layer_point_next_test() tests tetrahedron_lattice_layer_point_next().
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license. 
%
%  Modified:
%
%    06 July 2009
%
%  Author:
%
%    John Burkardt
%
  n = 3;

  fprintf ( 1, '\n' );
  fprintf ( 1, 'tetrahedron_lattice_layer_point_next_test():\n' );
  fprintf ( 1, '  tetrahedron_lattice_layer_point_next() returns the next\n' );
  fprintf ( 1, '  point in a tetrahedron lattice layer defined by:\n' );
  fprintf ( 1, '\n' );
  fprintf ( 1, '    C(4) - 1 < X(1)/C(1) + X(2)/C(2) +X(3)/C(3) <= C(4).\n' );

  c(1) = 2;
  c(2) = 3;
  c(3) = 4;
  v(1:n) = 0;

  fprintf ( 1, '\n' );
  fprintf ( 1, '  N = %d\n', n );
  fprintf ( 1, '  C =         %4d  %4d  %4d\n', c(1:n) );

  for layer = 0 : 2

    fprintf ( 1, '\n' );
    fprintf ( 1, '  Layer %d\n', layer );
    fprintf ( 1, '\n' );

    c(4) = layer;
    more = 0;
    i = 0;

    while ( true );
      [ v, more ] = tetrahedron_lattice_layer_point_next ( c, v, more );
      if ( ~ more )
        fprintf ( 1, '  No more.\n' );
        break
      end
      i = i + 1;
      fprintf ( 1, '  %4d  %4d  %4d  %4d\n', i, v(1:n) );

    end

  end

  return
end
