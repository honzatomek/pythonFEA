function p = perm1_random2 ( n )

%*****************************************************************************80
%
%% perm1_random2() selects a random permutation of (1,...,N).
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license. 
%
%  Modified:
%
%    01 July 2021
%
%  Author:
%
%    Original FORTRAN77 version by James Filliben.
%    MATLAB version by John Burkardt.
%
%  Reference:
%
%    K L Hoffman, D R Shier,
%    Algorithm 564,
%    A Test Problem Generator for Discrete Linear L1 Approximation Problems,
%    ACM Transactions on Mathematical Software,
%    Volume 6, Number 4, December 1980, pages 615-617.
%
%  Input:
%
%    integer N, the number of elements of the array.
%
%  Output:
%
%    integer P(N), a permutation, in standard index form.
%
  if ( n < 1 )
    fprintf ( 1, '\n' );
    fprintf ( 1, 'PERM1_RANDOM2 - Fatal error!\n' );
    fprintf ( 1, '  Illegal input value of N  = %d\n', n );
    fprintf ( 1, '  N must be at least 1!\n' );
    error ( 'PERM1_RANDOM2 - Fatal error!' );
  end

  if ( n == 1 )
    p(1) = 1;
    return
  end

  p = i4vec_indicator1 ( n );

  for i = 1 : n

    iadd = randi ( [ 1, n ], 1 );

    j = i + iadd;

    if ( n < j )
      j = j - n;
    end

    if ( i ~= j )
      t    = p(j);
      p(j) = p(i);
      p(i) = t;
    end

  end

  return
end
