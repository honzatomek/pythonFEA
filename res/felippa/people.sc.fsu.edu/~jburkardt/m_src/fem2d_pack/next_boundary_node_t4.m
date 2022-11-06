function next = next_boundary_node_t4 ( node )

%*****************************************************************************80
%
%% next_boundary_node_t4() returns the next boundary node in a T4 element.
%
%  Reference Element T4:
%
%    |
%    1  3
%    |  |\
%    |  | \
%    S  |  \
%    |  | 4 \
%    |  |    \
%    0  1-----2
%    |
%    +--0--R--1-->
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    14 February 2003
%
%  Author:
%
%    John Burkardt
%
%  Input:
%
%    integer NODE, the index of the current node.  An input
%    value of 0 (or any "unusual" value") indicates that the
%    first edge node is desired.
%
%  Output:
%
%    integer NEXT, the index of the next edge node.
%
  if ( node == 1 )
    next = 2;
  elseif ( node == 2 )
    next = 3;
  else
    next = 1;
  end

  return
end
