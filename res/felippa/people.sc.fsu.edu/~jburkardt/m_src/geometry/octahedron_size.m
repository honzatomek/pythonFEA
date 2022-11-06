function [ point_num, edge_num, face_num, face_order_max ] = octahedron_size ( )

%*****************************************************************************80
%
%% octahedron_size() returns size information for an octahedron.
%
%  Discussion:
%
%    This routine can be called before calling OCTAHEDRON_SHAPE,
%    so that space can be allocated for the arrays.
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
%  Output:
%
%    integer POINT_NUM, the number of points.
%
%    integer EDGE_NUM, the number of edges.
%
%    integer FACE_NUM, the number of faces.
%
%    integer FACE_ORDER_MAX, the maximum number of vertices 
%    per face.
%
  point_num = 6;
  edge_num = 12;
  face_num = 8;
  face_order_max = 3;

  return
end
