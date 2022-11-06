function t = subset_colex_unrank ( rank, n )

%*****************************************************************************80
%
%% subset_colex_unrank() computes the subset of given colexicographic rank.
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
%    integer RANK, the rank of the subset.
%
%    integer N, the number of items in the master set.
%    N must be positive.
%
%  Output:
%
%    integer T(N), the subsetof the given rank.
%    If T(I) = 0, item I is not in the subset; if T(I) = 1, item I is
%    in the subset.
%

%
%  Check.
%
  if ( n < 1 )
    fprintf ( 1, '\n' );
    fprintf ( 1, 'SUBSET_COLEX_UNRANK - Fatal error!\n' );
    fprintf ( 1, '  Input N is illegal.\n' );
    error ( 'SUBSET_COLEX_UNRANK - Fatal error!' );
  end

  nsub = subset_enum ( n );

  if ( rank < 0 | nsub < rank )
    fprintf ( 1, '\n' );
    fprintf ( 1, 'SUBSET_COLEX_UNRANK - Fatal error!\n' );
    fprintf ( 1, '  The input rank is illegal.\n' );
    error ( 'SUBSET_COLEX_UNRANK - Fatal error!' );
  end

  for i = 1 : n
    t(i) = mod ( rank, 2 );
    rank = floor ( rank / 2 );
  end

  return
end
