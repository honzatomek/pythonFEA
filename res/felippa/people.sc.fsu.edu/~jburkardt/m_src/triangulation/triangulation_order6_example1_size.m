function [ node_num, triangle_num, hole_num ] = ...
  triangulation_order6_example1_size ( )

%*****************************************************************************80
%
%% triangulation_order6_example1_size() sets sizes for a sample triangulation.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    13 June 2005
%
%  Author:
%
%    John Burkardt
%
%  Output:
%
%    integer NODE_NUM, the number of points.
%
%    integer TRIANGLE_NUM, the number of triangles.
%
%    integer HOLE_NUM, the number of holes.
%
  node_num = 48;
  triangle_num = 16;
  hole_num = 1;

  return
end
