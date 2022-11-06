function weight = subset_weight ( n, t )

%*****************************************************************************80
%
%% subset_weight() computes the Hamming weight of a set.
%
%  Discussion:
%
%    The Hamming weight is simply the number of elements in the set.
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
%    integer N, the order of the master set, of which T
%    is a subset.  N must be positive.
%
%    integer T(N), defines the subset T.
%    T(I) is 1 if I is an element of T, and 0 otherwise.
%
%  Output:
%
%    integer WEIGHT, the Hamming weight of the subset T.
%

%
%  Check.
%
  check = subset_check ( n, t );

  if ( ~ check )
    fprintf ( 1, '\n' );
    fprintf ( 1, 'SUBSET_WEIGHT - Fatal error!\n' );
    fprintf ( 1, '  The subset is not legal.\n' );
    error ( 'SUBSET_WEIGHT - Fatal error!\n' );
  end

  weight = sum ( t(1:n) );

  return
end
