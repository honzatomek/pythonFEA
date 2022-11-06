function c = subset_union ( n, a, b )

%*****************************************************************************80
%
%% subset_union() computes the union of two sets.
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
%    integer N, the order of the master set, of which A and
%    B are subsets.  N must be positive.
%
%    integer A(N), B(N), two subsets of the master set.
%    A(I) = 0 if the I-th element is in the subset A, and is
%    1 otherwise; B is defined similarly.
%
%  Output:
%
%    integer C(N), the union of A and B.
%

%
%  Check.
%
  check = subset_check ( n, a );

  if ( ~ check )
    fprintf ( 1, '\n' );
    fprintf ( 1, 'SUBSET_UNION - Fatal error!\n' );
    fprintf ( 1, '  The subset is not legal.\n' );
    error ( 'SUBSET_UNION - Fatal error!\n' );
  end

  check = subset_check ( n, b );

  if ( ~ check )
    fprintf ( 1, '\n' );
    fprintf ( 1, 'SUBSET_UNION - Fatal error!\n' );
    fprintf ( 1, '  The subset is not legal.\n' );
    error ( 'SUBSET_UNION - Fatal error!\n' );
  end

  for i = 1 : n
    c(i) = max ( a(i), b(i) );
  end

  return
end
