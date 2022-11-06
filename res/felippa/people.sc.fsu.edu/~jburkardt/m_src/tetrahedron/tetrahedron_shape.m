function [ point_coord, face_order, face_point ] = tetrahedron_shape ( ...
  point_num, face_num, face_order_max )

%*****************************************************************************80
%
%% tetrahedron_shape_3d() describes a tetrahedron.
%
%  Discussion:
%
%    Call tetrahedron_size() first, to get dimension information.
%
%    The vertices lie on the unit sphere.
%
%    The dual of the tetrahedron is the tetrahedron.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    30 January 2005
%
%  Author:
%
%    John Burkardt
%
%  Input:
%
%    integer POINT_NUM, the number of points in the shape.
%
%    integer FACE_NUM, the number of faces in the shape.
%
%    integer FACE_ORDER_MAX, the maximum number of vertices per face.
%
%  Output:
%
%    real POINT_COORD(3,POINT_NUM), the vertices.
%
%    integer FACE_ORDER(FACE_NUM), the number of vertices
%    for each face.
%
%    integer FACE_POINT(FACE_ORDER_MAX,FACE_NUM); FACE_POINT(I,J)
%    contains the index of the I-th point in the J-th face.  The
%    points are listed in the counter-clockwise direction defined
%    by the outward normal at the face.
%
  dim_num = 3;
%
%  Set the point coordinates.
%
  point_coord(1:dim_num,1:point_num) = [ ...
        0.942809,    0.000000,   -0.333333; ...
       -0.471405,    0.816497,   -0.333333; ...
       -0.471405,   -0.816497,   -0.333333; ...
        0.000000,    0.000000,    1.000000 ]';
%
%  Set the face orders.
%
  face_order(1:face_num) = [ 3, 3, 3, 3 ]';
%
%  Set faces.
%
  face_point(1:face_order_max,1:face_num) = [ ...
       1, 3, 2; ...
       1, 2, 4; ...
       1, 4, 3; ...
       2, 3, 4 ]';

  return
end
