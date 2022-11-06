function p_inv = perm1_inverse3 ( n, p )

%*****************************************************************************80
%
%% perm1_inverse3() produces the inverse of a given permutation.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license. 
%
%  Modified:
%
%    05 January 2007
%
%  Author:
%
%    John Burkardt
%
%  Input:
%
%    integer N, the number of items permuted.
%
%    integer P(N), a permutation.
%
%  Output:
%
%    integer P_INV(N), the inverse permutation.
%
  ierror = perm1_check ( n, p );

  if ( ierror )
    fprintf ( 1, '\n' );
    fprintf ( 1, 'PERM1_INVERSE3 - Fatal error!\n' );
    fprintf ( 1, '  The input array does not represent\n' );
    fprintf ( 1, '  a proper permutation.  In particular, the\n' );
    fprintf ( 1, '  array is missing the value %d\n', ierror );
    error ( 'PERM1_INVERSE3 - Fatal error!' );
  end

  p_inv(p(1:n)) = ( 1 : n );

  return
end
