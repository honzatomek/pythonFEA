function [ point_num, edge_num, face_num, face_order_max ] = soccer_size ( )

%*****************************************************************************80
%
%% soccer_size() gives "sizes" for a truncated icosahedron.
%
%  Discussion:
%
%    The shape is a truncated icosahedron, which is the design used
%    on a soccer ball.  There are 12 pentagons and 20 hexagons.
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
%  Reference:
%
%    http://polyhedra.wolfram.com/uniform/u25.html
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
  point_num = 60;
  edge_num = 90;
  face_num = 32;
  face_order_max = 6;

  return
end
