function polyhedron_volume_3d_test ()

%*****************************************************************************80
%
%% polyhedron_volume_3d_test() tests polyhedron_volume_3d().
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    19 March 2009
%
%  Author:
%
%    John Burkardt
%
  maxorder = 3;
  face_num = 4;
  dim_num = 3;
  node_num = 4;

  coord = [ ...
    0.0, 0.0, 0.0; ...
    1.0, 0.0, 0.0; ...
    0.0, 1.0, 0.0; ...
    0.0, 0.0, 1.0 ]';
  node = [ ...
    3, 1, 1, 2; ...
    2, 2, 4, 3; ...
    1, 4, 3, 4 ]';
  order = [ 3, 3, 3, 3 ];
  volume_exact = 1.0 / 6.0;
 
  fprintf ( 1, '\n' );
  fprintf ( 1, 'polyhedron_volume_3d_test():\n' );
  fprintf ( 1, '  polyhedron_volume_3d() computes the volume\n' );
  fprintf ( 1, '  of a polyhedron in 3D.\n' );
  fprintf ( 1, '\n' );
  fprintf ( 1, '  Number of faces is %d\n', face_num );

  i4vec_print ( face_num, order, '  Order of each face:' );

  fprintf ( 1, '\n' );
  fprintf ( 1, '  Nodes per face:\n' );
  fprintf ( 1, '\n' );

  for i = 1 : face_num
    fprintf ( 1, '  %4d', i );
    for j = 1 : order(i)
      fprintf ( 1, '  %4d', node(i,j) );
    end
    fprintf ( 1, '\n' );
  end

  r8mat_transpose_print ( dim_num, node_num, coord, '  Polyhedron nodes' );

  volume = polyhedron_volume_3d ( coord, maxorder, face_num, node, node_num, ...
    order );

  fprintf ( 1, '\n' );
  fprintf ( 1, '  Volume ( computed ) = %f\n', volume );
  fprintf ( 1, '  Volume ( exact ) =    %f\n', volume_exact );

  return
end
