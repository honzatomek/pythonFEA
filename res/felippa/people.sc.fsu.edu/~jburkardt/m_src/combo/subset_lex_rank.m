function rank = subset_lex_rank ( n, t )

%*****************************************************************************80
%
%% subset_lex_rank() computes the lexicographic rank of a subset.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    22 August 2011
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
%    integer N, the number of items in the master set.
%    N must be positive.
%
%    integer T(N), the subset.  If T(I) = 0, item I is
%    not in the subset; if T(I) = 1, item I is in the subset.
%
%  Output:
%
%    integer RANK, the rank of the subset.
%

%
%  Check.
%
  check = subset_check ( n, t );

  if ( ~ check )
    fprintf ( 1, '\n' );
    fprintf ( 1, 'SUBSET_LEX_RANK - Fatal error!\n' );
    fprintf ( 1, '  The subset is not legal.\n' );
    error ( 'SUBSET_LEX_RANK - Fatal error!\n' );
  end

  rank = 0;

  for i = 1 : n

    if ( t(i) == 1 )
      rank = rank + 2^( n - i );
    end

  end

  return
end
