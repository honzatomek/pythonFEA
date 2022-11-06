function edge_num = edge_enum ( node_num )

%*****************************************************************************80
%
%% edge_enum() enumerates the maximum number of edges in a graph on NODE_NUM nodes.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    22 January 2011
%
%  Author:
%
%    John Burkardt
%
%  Input:
%
%    integer NODE_NUM, the number of nodes in the graph.
%    N_NODE must be positive.
%
%  Output:
%
%    integer EDGE_NUM, the maximum number of edges in a graph
%    on N_NODE nodes.
%
  edge_num = ( node_num * ( node_num - 1 ) ) / 2;

  return
end
