function rank = ksubset_colex_rank ( k, n, t )

%*****************************************************************************80
%
%% ksubset_colex_rank() computes the colex rank of a K subset.
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
%    integer K, the number of elements each K subset must
%    have.  1 <= K <= N.
%
%    integer N, the number of elements in the master set.
%    N must be positive.
%
%    integer T(K), describes a K subset.  T(I) is the I-th
%    element of the K subset.  The elements must be listed in DESCENDING order.
%
%  Output:
%
%    integer RANK, the rank of the subset.
%

%
%  Check.
%
  check = ksubset_colex_check ( k, n, t );

  if ( ~ check )
    fprintf ( 1, '\n' );
    fprintf ( 1, 'KSUBSET_COLEX_RANK - Fatal error!\n' );
    fprintf ( 1, '  The input array is illegal.\n' );
    error ( 'KSUBSET_COLEX_RANK - Fatal error!' );
  end

  rank = 0;

  for i = 1 : k
    rank = rank + i4_choose ( t(i) - 1, k + 1 - i );
  end

  return
end
