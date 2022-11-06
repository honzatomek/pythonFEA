function check = tree_check ( n, t )

%*****************************************************************************80
%
%% tree_check() checks a tree.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    03 December 2015
%
%  Author:
%
%    John Burkardt
%
%  Reference:
%
%    Donald Kreher, Douglas Simpson,
%    Combinatorial Algorithms,
%    CRC Press, 1998,
%    ISBN: 0-8493-3988-X,
%    LC: QA164.K73.
%
%  Input:
%
%    integer N, the number of nodes in the tree.
%    N must be positive.
%
%    integer T(2,N-1), describes the edges of the tree
%    as pairs of nodes.
%
%  Output:
%
%    integer CHECK, error flag.
%    1, the data is legal.
%    0, the data is not legal.
%
  check = 1;

  if ( n < 1 )
    check = 0;
    return
  end

  for i = 1 : 2
    for j = 1 : n - 1
      if ( t(i,j) < 1 | n < t(i,j) )
        check = 0;
        return
      end
    end
  end
%
%  Compute the degree of each node.
%
  d = edge_degree ( n, n-1, t );
%
%  Delete a node of degree 1, N-1 times.
%
  for k = 1 : n - 1

    x = 1;

    while ( d(x) ~= 1 )
      x = x + 1;
      if ( n < x )
        check = 0;
        return
      end
    end
%
%  Find its neighbor.
%
    j = 1;

    while ( true )

      if ( t(1,j) == x )
        y = t(2,j);
        break
      end

      if ( t(2,j) == x )
        y = t(1,j);
        break
      end

      j = j + 1;

      if ( n - 1 < j )
        check = 0;
        return
      end

    end
%
%  Delete the edge.
%
    t(1,j) = - t(1,j);
    t(2,j) = - t(2,j);

    d(x) = d(x) - 1;
    d(y) = d(y) - 1;

  end

  return
end
