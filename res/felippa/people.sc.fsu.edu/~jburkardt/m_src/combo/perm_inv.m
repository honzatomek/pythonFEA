function pinv = perm_inv ( n, p )

%*****************************************************************************80
%
%% perm_inv() computes the inverse of a permutation.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    12 January 2011
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
%    integer N, the number of values being permuted.
%    N must be positive.
%
%    integer P(N), describes the permutation.
%    P(I) is the item which is permuted into the I-th place
%    by the permutation.
%
%  Output:
%
%    integer PINV(N), the inverse permutation.
%

%
%  Check.
%
  check = perm_check ( n, p );

  if ( ~ check )
    fprintf ( 1, '\n' );
    fprintf ( 1, 'PERM_INV - Fatal error!\n' );
    fprintf ( 1, '  The input array is illegal.\n' );
    error ( 'PERM_INV - Fatal error!' );
  end

  pinv(p(1:n)) = 1:n;

  return
end
