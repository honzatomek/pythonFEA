function [ t, rank ] = tree_successor ( n, t, rank )

%*****************************************************************************80
%
%% tree_successor() returns the successor of a tree.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    19 January 2011
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
%    N must be at least 3.
%
%    integer T(2,N-1), describes the edges of the
%    tree as pairs of nodes.  
%
%    integer RANK, the rank of the tree.
%
%  Output:
%
%    integer T(2,N-1), describes the successor tree.
%
%    integer RANK, the rank of the successor tree.
%

%
%  Return the first element.
%
  if ( rank == -1 )
    p = ones ( n - 2, 1 );
    t = pruefer_to_tree ( n, p );
    rank = 0;
    return
  end
%
%  Check the tree.
%
  check = tree_check ( n, t );

  if ( ~ check )
    fprintf ( 1, '\n' );
    fprintf ( 1, 'TREE_SUCCESSOR - Fatal error!\n' );
    fprintf ( 1, '  Input tree is illegal.\n' );
    error ( 'TREE_SUCCESSOR - Fatal error!' );
  end
%
%  Convert the tree to a Pruefer code.
%
  p = tree_to_pruefer ( n, t );
%
%  Find the successor of the Pruefer code.
%
  [ p, rank ] = pruefer_successor ( n, p, rank );
%
%  Convert the Pruefer code to the tree.
%
  t = pruefer_to_tree ( n, p );

  return
end
