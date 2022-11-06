function dual_shape_test ( )

%*****************************************************************************80
%
%% dual_shape_test() tests dual_shape();
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
  fprintf ( 1, '\n' );
  fprintf ( 1, 'dual_shape_test():\n' );
  fprintf ( 1, '  dual_shape() finds the dual of a polyhedron.\n' );
%
%  Get the dodecahedron shape.
%
  [ point_num1, edge_num1, face_num1, face_order_max1 ] = dodec_size ( );

  fprintf ( 1, '\n' );
  fprintf ( 1, '  Number of points =   %d\n', point_num1 );
  fprintf ( 1, '  Number of edges =    %d\n', edge_num1 );
  fprintf ( 1, '  Number of faces =    %d\n', face_num1 );
  fprintf ( 1, '  Maximum face order = %d\n', face_order_max1 );

  [ point_coord1, face_order1, face_point1 ] = dodec_shape ( point_num1, ...
    face_num1, face_order_max1 );
%
%  Get the dual.
%
  [ point_num2, edge_num2, face_num2, face_order_max2 ] = dual_size ( ...
    point_num1, edge_num1, face_num1, face_order_max1, point_coord1, ...
    face_order1, face_point1 );

  [ point_coord2, face_order2, face_point2 ] = dual_shape ( point_num1, ...
    face_num1, face_order_max1, point_coord1, face_order1, face_point1, ...
    point_num2, face_num2, face_order_max2 );

  fprintf ( 1, '\n' );
  fprintf ( 1, '  Number of points =   %d\n', point_num2 );
  fprintf ( 1, '  Number of edges =    %d\n', edge_num2 );
  fprintf ( 1, '  Number of faces =    %d\n', face_num2 );
  fprintf ( 1, '  Maximum face order = %d\n', face_order_max2 );

  shape_print ( point_num2, face_num2, face_order_max2, ...
    point_coord2, face_order2, face_point2 );

  return
end
