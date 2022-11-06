function t = ksubset_lex_unrank ( rank, k, n )

%*****************************************************************************80
%
%% ksubset_lex_unrank() computes the K subset of given lexicographic rank.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    11 January 2021
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
%    integer RANK, the rank of the K subset.
%
%    integer K, the number of elements each K subset must
%    have.  0 <= K <= N.
%
%    integer N, the number of elements in the master set.
%    N must be positive.
%
%  Output:
%
%    integer T(K), describes the K subset of the given
%    rank.  T(I) is the I-th element.  The elements must be listed in
%    ascending order.
%

%
%  Check.
%
  if ( n < 1 )
    fprintf ( 1, '\n' );
    fprintf ( 1, 'KSUBSET_LEX_RANK - Fatal error!\n' );
    fprintf ( 1, '  Input N is illegal.\n' );
    error ( 'KSUBSET_LEX_UNRANK - Fatal error!' );
  end

  if ( k == 0 )
    t = zeros ( k, 1 );
    return
  end

  if ( k < 0 | n < k )
    fprintf ( 1, '\n' );
    fprintf ( 1, 'KSUBSET_LEX_RANK - Fatal error!\n' );
    fprintf ( 1, '  Input K is illegal.\n' );
    error ( 'KSUBSET_LEX_UNRANK - Fatal error!' );
  end

  nksub = ksubset_enum ( k, n );

  if ( rank < 0 | nksub < rank )
    fprintf ( 1, '\n' );
    fprintf ( 1, 'KSUBSET_LEX_UNRANK - Fatal error!\n' );
    fprintf ( 1, '  Input rank is illegal.\n' );
    error ( 'KSUBSET_LEX_UNRANK - Fatal error!' );
  end

  t = zeros ( k, 1 );

  x = 1;

  for i = 1 : k

    while ( nchoosek ( n - x, k - i ) <= rank )
      rank = rank - nchoosek ( n - x, k - i );
      x = x + 1;
    end

    t(i) = x;
    x = x + 1;

  end

  return
end
