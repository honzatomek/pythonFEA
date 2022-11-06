function p3 = perm1_mul ( n, p1, p2 )

%*****************************************************************************80
%
%% perm1_mul() "multiplies" two permutations of (1,...,N).
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license. 
%
%  Modified:
%
%    01 August 2004
%
%  Author:
%
%    John Burkardt
%
%  Input:
%
%    integer N, the order of the permutations.
%
%    integer P1(N), P2(N), the permutations, in standard index form.
%
%  Output:
%
%    integer P3(N), the product permutation.
%
  ierror = perm1_check ( n, p1 );

  if ( ierror )
    fprintf ( 1, '\n' );
    fprintf ( 1, 'PERM1_MUL - Fatal error!\n' );
    fprintf ( 1, '  The input array P1 does not represent\n' );
    fprintf ( 1, '  a proper permutation.  In particular, the\n' );
    fprintf ( 1, '  array is missing the value %d\n', ierror );
    error ( 'PERM1_MUL - Fatal error!' );
  end

  ierror = perm1_check ( n, p2 );

  if ( ierror )
    fprintf ( 1, '\n' );
    fprintf ( 1, 'PERM1_MUL - Fatal error!\n' );
    fprintf ( 1, '  The input array P2 does not represent\n' );
    fprintf ( 1, '  a proper permutation.  In particular, the\n' );
    fprintf ( 1, '  array is missing the value %d\n', ierror );
    error ( 'PERM1_MUL - Fatal error!' );
  end

  p3(1:n) = p2(p1(1:n));

  return
end
