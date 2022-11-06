function t = tree_random ( n )

%*****************************************************************************80
%
%% tree_random() randomly selects a tree on N vertices.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    11 September 2022
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
%  Output:
%
%    integer T(2,N-1), describes the edges of the tree as pairs of nodes.
%

%
%  Check the value of N.
%
  if ( n < 1 )
    fprintf ( 1, '\n' );
    fprintf ( 1, 'tree_random(): Fatal error!\n' );
    fprintf ( 1, '  Input N is illegal.\n' );
    error ( 'tree_random(): Fatal error!' );
  end
%
%  Compute the number of trees.
%
  tree_num = tree_enum ( n );
%
%  Choose RANK betweeen 1 and TREE_NUM.
%
  rank = randi ( [ 1, tree_num ], 1, 1 );
%
%  Compute the Pruefer code P of the given RANK.
%
  p = pruefer_unrank ( rank, n );
%
%  Convert the Pruefer code P to a tree T.
%
  t = pruefer_to_tree ( n, p );

  return
end
