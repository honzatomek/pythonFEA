function tetrahedron_edge_length_test ( )

%*****************************************************************************80
%
%% tetrahedron_edge_length_test() tests tetrahedron_edge_length().
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    14 February 2003
%
%  Author:
%
%    John Burkardt
%
  tetra = [ ...
     0.577350269189626,  0.0, 0.0; ...
    -0.288675134594813,  0.5, 0.0; ...
    -0.288675134594813, -0.5, 0.0; ...
     0.0,                0.0, 0.816496580927726 ]';

  fprintf ( 1, '\n' );
  fprintf ( 1, 'tetrahedron_edge_length_test():\n' );
  fprintf ( 1, '  tetrahedron_edge_length() computes the edge lengths;\n' );

  fprintf ( 1, '\n' );
  fprintf ( 1, '  tetrahedron vertices:\n' );
  disp ( tetra );

  edge_length = tetrahedron_edge_length ( tetra );

  fprintf ( 1, '\n' );
  fprintf ( 1, '  Edge lengths:\n' );
  disp ( edge_length );

  return
end
