function check = edge_check ( n_node, n_edge, t )

%*****************************************************************************80
%
%% edge_check() checks a graph stored by edges.
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
%    integer  N_NODE, the number of nodes in the graph.
%
%    integer N_EDGE, the number of edges in the graph.
%
%    integer T(2,N_EDGE), describes the edges of the tree
%    as pairs of nodes.
%
%  Output:
%
%    integer CHECK
%    1, the data is legal.
%    0, the data is not legal. 
%
  check = 1;

  if ( n_node < 0 )
    check = 0;
    return
  end

  if ( n_edge < 0 )
    check = 0;
    return
  end
%
%  Every edge must join two legal nodes.
%
  for i = 1 : 2
    for j = 1 : n_edge
      if ( t(i,j) < 1 | n_node < t(i,j) )
        check = 0;
        return
      end
    end
  end
%
%  Every edge must join distinct nodes.
%
  for j = 1 : n_edge
    if ( t(1,j) == t(2,j) )
      check = 0;
      return
    end
  end
%
%  Every edge must be distinct.
%
  for j = 1 : n_edge - 1
    for j2 = j + 1 : n_edge
      if ( t(1,j) == t(1,j2) & t(2,j) == t(2,j2) )
        check = 0;
        return
      elseif ( t(1,j) == t(2,j2) & t(2,j) == t(1,j2) )
        check = 0;
        return
      end
    end
  end

  return
end
