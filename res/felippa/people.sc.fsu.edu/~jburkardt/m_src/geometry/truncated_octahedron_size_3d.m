function [ point_num, edge_num, face_num, face_order_max ] = ...
  truncated_octahedron_size_3d ( )

%*****************************************************************************80
%
%% truncated_octahedron_size_3d() gives "sizes" for a truncated octahedron in 3D.
%
%  Discussion:
%
%    The truncated octahedron is "space-filling".
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
%    integer FACE_ORDER_MAX, the maximum order of any face.
%
  point_num = 24;
  edge_num = 36;
  face_num = 14;
  face_order_max = 6;

  return
end
