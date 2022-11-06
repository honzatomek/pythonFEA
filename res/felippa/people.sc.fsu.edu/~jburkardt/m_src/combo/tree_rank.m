function rank = tree_rank ( n, t )

%*****************************************************************************80
%
%% tree_rank() ranks a tree.
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
%    N must be at least 3.
%
%    integer T(2,N-1), describes the edges of the tree
%    as pairs of nodes.
%
%  Output:
%
%    integer RANK, the rank of the tree.
%

%
%  Check the tree.
%
  check = tree_check ( n, t );

  if ( ~ check )
    fprintf ( 1, '\n' );
    fprintf ( 1, 'TREE_RANK - Fatal error!\n' );
    fprintf ( 1, '  Input tree is illegal.\n' );
    error ( 'TREE_RANK - Fatal error!' );
  end
%
%  Convert the tree to a Pruefer code.
%
  p = tree_to_pruefer ( n, t );
%
%  Find the rank of the Pruefer code.
%
  rank = pruefer_rank ( n, p );

  return
end
