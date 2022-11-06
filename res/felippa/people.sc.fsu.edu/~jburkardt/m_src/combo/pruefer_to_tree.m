function t = pruefer_to_tree ( n, p )

%*****************************************************************************80
%
%% pruefer_to_tree() converts a Pruefer code to a tree.
%
%  Discussion:
%
%    The original code attempts to tack on an extra entry to P.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    25 January 2011
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
%    integer P(N-2), the Pruefer code for the tree.
%
%  Output:
%
%    integer T(2,N-1), describes the edges of the tree
%    as pairs of nodes.
%

%
%  Check.
%
  check = pruefer_check ( n, p );

  if ( ~ check )
    fprintf ( 1, '\n' );
    fprintf ( 1, 'PRUEFER_TO_TREE - Fatal error!\n' );
    fprintf ( 1, '  Input array is illegal.\n' );
    error ( 'PRUEFER_TO_TREE - Fatal error!' );
  end
%
%  Initialize the tree to 0.
%
  t = zeros ( 2, n - 1 );

  d(1:n) = 1;

  for i = 1 : n - 2
    d(p(i)) = d(p(i)) + 1;
  end

  for i = 1 : n - 1

    x = n;
    while ( d(x) ~= 1 )
      x = x - 1;
    end

    if ( i == n - 1 )
      y = 1;
    else
      y = p(i);
    end

    d(x) = d(x) - 1;
    d(y) = d(y) - 1;

    t(1,i) = x;
    t(2,i) = y;

  end

  return
end
