function dist = subset_distance_hamming ( n, t1, t2 )

%*****************************************************************************80
%
%% subset_distance_hamming() computes the Hamming distance between two sets.
%
%  Discussion:
%
%    The sets T1 and T2 are assumed to be subsets of a set of N elements.
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
%    integer N, the order of the master set, of which T1 and
%    T2 are subsets.  N must be positive.
%
%    integer T1(N), T2(N), two subsets of the master set.
%    T1(I) = 0 if the I-th element is in the subset T1, and is
%    1 otherwise; T2 is defined similarly.
%
%  Output:
%
%    integer DIST, the Hamming distance between T1 and T2,
%    defined as the number of elements of the master set which are
%    in either T1 or T2 but not both.
%

%
%  Check.
%
  check = subset_check ( n, t1 );

  if ( ~ check )
    fprintf ( 1, '\n' );
    fprintf ( 1, 'subset_distance_hamming - Fatal error!\n' );
    fprintf ( 1, '  The subset T1 is not legal.\n' );
    error ( 'subset_distance_hamming - Fatal error!\n' );
  end

  check = subset_check ( n, t2 );

  if ( ~ check )
    fprintf ( 1, '\n' );
    fprintf ( 1, 'subset_distance_hamming - Fatal error!\n' );
    fprintf ( 1, '  The subset T2 is not legal.\n' );
    error ( 'subset_distance_hamming - Fatal error!\n' );
  end

  dist = sum ( t1(1:n) ~= t2(1:n) );

  return
end
