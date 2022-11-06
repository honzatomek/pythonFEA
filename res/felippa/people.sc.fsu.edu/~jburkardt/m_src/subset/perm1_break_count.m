function break_count = perm1_break_count ( n, p )

%*****************************************************************************80
%
%% perm1_break_count() counts breaks in a permutation of (1,...,N).
%
%  Discussion:
%
%    We begin with a permutation of order N.  We prepend an element
%    labeled "0" and append an element labeled "N+1".  There are now
%    N+1 pairs of neighbors.  A "break" is a pair of neighbors whose
%    value differs by more than 1.  
%
%    The identity permutation has a break count of 0.  The maximum
%    break count is N+1.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license. 
%
%  Modified:
%
%    16 June 2004
%
%  Author:
%
%    John Burkardt
%
%  Input:
%
%    integer N, the order of the permutation.
%
%    integer P(N), a permutation, in standard index form.
%
%  Output:
%
%    integer BREAK_COUNT, the number of breaks in the permutation.
%
  break_count = 0;
%
%  Make sure the permutation is a legal one.
%  (This is not an efficient way to do so!)
%
  ierror = perm1_check ( n, p );

  if ( ierror ~= 0 )
    fprintf ( 1, '\n' );
    fprintf ( 1, 'PERM1_BREAK_COUNT - Fatal error!\n' );
    fprintf ( 1, '  The input array does not represent\n' );
    fprintf ( 1, '  a proper permutation.  In particular, the\n' );
    fprintf ( 1, '  array is missing the value %d\n', ierror );
    error ( 'PERM1_BREAK_COUNT - Fatal error!' );
  end

  if ( p(1) ~= 1 )
    break_count = break_count + 1;
  end

  for i = 1 : n-1
    if ( abs ( p(i+1) - p(i) ) ~= 1 )
      break_count = break_count + 1;
    end
  end

  if ( p(n) ~= n )
    break_count = break_count + 1;
  end

  return
end
