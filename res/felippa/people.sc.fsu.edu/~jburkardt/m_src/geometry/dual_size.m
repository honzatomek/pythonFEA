function [ point_num2, edge_num2, face_num2, face_order_max2 ] = ...
  dual_size ( point_num, edge_num, face_num, face_order_max, ...
  point_coord, face_order, face_point )

%*****************************************************************************80
%
%% dual_size() determines sizes for a dual of a shape.
%
%  Discussion:
%
%    We don't actually need FACE_POINT as input here.  But since the
%    three arrays occur together everywhere else, it seems unnecessarily
%    user-confusing to vary the usage here!
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    22 July 2007
%
%  Author:
%
%    John Burkardt
%
%  Input:
%
%    integer POINT_NUM, the number of points.
%
%    integer EDGE_NUM, the number of edges.
%
%    integer FACE_NUM, the number of faces.
%
%    integer FACE_ORDER_MAX, the maximum number of vertices per face.
%
%    real POINT_COORD(3,POINT_NUM); POINT_COORD(*,J) is
%    the X, Y and Z coordinates of point J.
%
%    integer FACE_ORDER(FACE_NUM), the number of vertices per face.
%
%    integer FACE_POINT(FACE_ORDER_MAX,FACE_NUM); FACE_POINT(I,J)
%    is the index of the I-th point in the J-th face.  The
%    points are listed in the counter-clockwise direction defined
%    by the outward normal at the face.
%
%  Output:
%
%    integer POINT_NUM2, the number of points in the dual.
%
%    integer EDGE_NUM2, the number of edges in the dual.
%
%    integer FACE_NUM2, the number of faces in the dual.
%
%    integer FACE_ORDER_MAX2, the maximum number of vertices per face
%    in the dual.
%

%
%  These values are easy to compute:
%
  point_num2 = face_num;
  edge_num2 = edge_num;
  face_num2 = point_num;
%
%  To determine FACE_ORDER_MAX2 is not so easy.
%  You have to construct the FACE_ORDER array for the dual shape.
%  The order of a dual face is the number of edges that the vertex occurs in.
%  But then all we have to do is count how many times each item shows up
%  in the FACE_POINT array.
%
  face_order_max2 = 0;
  face_order2(1:face_num2) = 0;

  for face = 1 : face_num
    for i = 1 : face_order(face)
      face2 = face_point(i,face);
      face_order2(face2) = face_order2(face2) + 1;
    end
  end

  face_order_max2 = max ( face_order2(1:face_num2) );

  return
end
