function [ point_num, edge_num, face_num, face_order_max ] = cube_size ( )

%*****************************************************************************80
%
%% cube_size() gives "sizes" for a cube.
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
  point_num = 8;
  edge_num = 12;
  face_num = 6;
  face_order_max = 4;

  return
end
