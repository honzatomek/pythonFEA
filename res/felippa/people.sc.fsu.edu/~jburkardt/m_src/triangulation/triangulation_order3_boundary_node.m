function node_boundary = triangulation_order3_boundary_node ( node_num, ...
  triangle_num, triangle_node )

%*****************************************************************************80
%
%% triangulation_order3_boundary_node() indicates which nodes are on the boundary.
%
%  Discussion:
%
%    This routine is given a triangulation, an abstract list of triples
%    of nodes.  It is assumed that the nodes in each triangle are listed
%    in a counterclockwise order, although the routine should work
%    if the nodes are consistently listed in a clockwise order as well.
%
%    It is assumed that each edge of the triangulation is either
%    * an INTERIOR edge, which is listed twice, once with positive
%      orientation and once with negative orientation, or;
%    * a BOUNDARY edge, which will occur only once.
%
%    This routine should work even if the region has holes - as long
%    as the boundary of the hole comprises more than 3 edges!
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    12 June 2005
%
%  Author:
%
%    John Burkardt
%
%  Input:
%
%    integer NODE_NUM, the number of nodes.
%
%    integer TRIANGLE_NUM, the number of triangles.
%
%    integer TRIANGLE_NODE(3,TRIANGLE_NUM), the nodes that make up the
%    triangles.  These should be listed in counterclockwise order.
%
%  Output:
%
%    logical NODE_BOUNDARY(NODE_NUM), is TRUE if the node
%    is on a boundary edge.
%
  m = 2;
  n = 3 * triangle_num;
%
%  Set up the edge array.
%
  edge(1:2,               1:  triangle_num) = triangle_node(1:2,1:triangle_num);
  edge(1:2,  triangle_num+1:2*triangle_num) = triangle_node(2:3,1:triangle_num);
  edge(1,  2*triangle_num+1:3*triangle_num) = triangle_node(3,  1:triangle_num);
  edge(2,  2*triangle_num+1:3*triangle_num) = triangle_node(1,  1:triangle_num);
%
%  In each column, force the smaller entry to appear first.
%
  e1(1:n) = min ( edge(1:2,1:n) );
  e2(1:n) = max ( edge(1:2,1:n) );

  edge(1,1:n) = e1(1:n);
  edge(2,1:n) = e2(1:n);
%
%  Ascending sort the column array.
%
  edge = ( sortrows ( edge' ) )';
%
%  Records which appear twice are internal edges and can be ignored.
%
  node_boundary(1:node_num) = 0;

  j = 0;

  while ( j < 3 * triangle_num )

    j = j + 1;

    if ( j == 3 * triangle_num )
      node_boundary(edge(1:m,j)) = 1;
    elseif ( all ( edge(1:m,j) == edge(1:m,j+1) ) )
      j = j + 1;
    else
      node_boundary(edge(1:m,j)) = 1;
    end

  end

  return
end
