function node_num = grid_t4_node_num ( nelemx, nelemy )

%*****************************************************************************80
%
%% grid_t4_node_num() counts the nodes in a grid of T4 elements.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    15 February 2006
%
%  Author:
%
%    John Burkardt
%
%  Input:
%
%    integer NELEMX, NELEMY, the number of elements along the
%    X and Y directions.  The number of elements generated will be
%    NELEMX * NELEMY.
%
%  Output:
%
%    integer NODE_NUM, the number of nodes in the grid.
%
  node_num = ( nelemx + 1 ) * ( nelemy + 1 ) + 2 * nelemx * nelemy;

  return
end
