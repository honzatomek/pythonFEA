function rank = ksubset_revdoor_rank ( k, n, t )

%*****************************************************************************80
%
%% ksubset_revdoor_rank() computes the revolving door rank of a K subset.
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
%    element.  The elements must be listed in ascending order.
%
%  Output:
%
%    integer RANK, the rank of the K subset.
%

%
%  Check.
%
  check = ksubset_lex_check ( k, n, t );

  if ( ~ check )
    fprintf ( 1, '\n' );
    fprintf ( 1, 'KSUBSET_REVDOOR_RANK - Fatal error!\n' );
    fprintf ( 1, '  The input array is illegal.\n' );
    error ( 'KSUBSET_REVDOOR_RANK - Fatal error!' );
  end

  if ( mod ( k, 2 ) == 0 )

    rank = 0;

  else

    rank = - 1;

  end

  s = 1;

  for i = k : -1 : 1
    rank = rank + s * nchoosek ( t(i), i );
    s = - s;
  end

  return
end
