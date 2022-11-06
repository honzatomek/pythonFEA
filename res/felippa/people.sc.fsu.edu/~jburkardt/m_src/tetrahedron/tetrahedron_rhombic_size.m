function [ point_num, edge_num, face_num, face_order_max ] = ...
  tetrahedron_rhombic_size ( )

%*****************************************************************************80
%
%% tetrahedron_rhombic_size() gives "sizes" for a rhombic tetrahedron.
%
%  Discussion:
%
%    Call this routine first, in order to learn the required dimensions
%    of arrays to be set up by TETRAHEDRON_RHOMBIC_SHAPE.
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
%    integer POINT_NUM, the number of vertices.
%
%    integer EDGE_NUM, the number of edges.
%
%    integer FACE_NUM, the number of faces.
%
%    integer FACE_ORDER_MAX, the maximum order of any face.
%
  point_num = 10;
  edge_num = 6;
  face_num = 4;
  face_order_max = 6;

  return
end
