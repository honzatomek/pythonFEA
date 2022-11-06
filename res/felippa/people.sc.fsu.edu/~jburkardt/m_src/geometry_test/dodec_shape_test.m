function dodec_shape_test ( )

%*****************************************************************************80
%
%% dodec_shape_test() tests dodec_shape().
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
  fprintf ( 1, 'dodec_shape_test():\n' );
  fprintf ( 1, '  For the dodecahedron,\n' );
  fprintf ( 1, '  dodec_size() returns dimension information;\n' );
  fprintf ( 1, '  dodec_shape() returns face and order information.\n' );
  fprintf ( 1, '  shape_print() prints this information.\n' );

  [ point_num, edge_num, face_num, face_order_max ] = dodec_size ( );

  fprintf ( 1, '\n' );
  fprintf ( 1, '  Number of points =   %d\n', point_num );
  fprintf ( 1, '  Number of edges =    %d\n', edge_num );
  fprintf ( 1, '  Number of faces =    %d\n', face_num );
  fprintf ( 1, '  Maximum face order = %d\n', face_order_max );

  [ point_coord, face_order, face_point ] = dodec_shape ( point_num, ...
    face_num, face_order_max );

  shape_print ( point_num, face_num, face_order_max, ...
    point_coord, face_order, face_point );

  return
end
