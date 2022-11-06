function p2 = perm1_inverse ( n, p1 )

%*****************************************************************************80
%
%% perm1_inverse() inverts a permutation of (1,...,N).
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license. 
%
%  Modified:
%
%    16 June 2015
%
%  Author:
%
%    John Burkardt
%
%  Input:
%
%    integer N, the number of objects being permuted.
%
%    integer P1(N), the permutation.
%
%  Output:
%
%    integer P2(N), the inverse permutation
%
  if ( n <= 0 )
    fprintf ( 1, '\n' );
    fprintf ( 1, 'PERM1_INVERSE - Fatal error!\n' );
    fprintf ( 1, '  Input value of N = %d\n', n );
    error ( 'PERM1_INVERSE - Fatal error!' );
  end

  ierror = perm1_check ( n, p1 );

  if ( ierror )
    fprintf ( 1, '\n' );
    fprintf ( 1, 'PERM1_INVERSE - Fatal error!\n' );
    fprintf ( 1, '  The input array does not represent\n' );
    fprintf ( 1, '  a proper permutation.  In particular, the\n' );
    fprintf ( 1, '  array is missing the value %d\n', ierror );
    error ( 'PERM1_INVERSE - Fatal error!' );
  end

  p2(1:n) = p1(1:n);

  is = 1;

  for i = 1 : n

    i1 = p2(i);

    while ( i < i1 )
      i2 = p2(i1);
      p2(i1) = -i2;
      i1 = i2;
    end

    is = - i4_sign ( p2(i) );
    p2(i) = i4_sign ( is ) * abs ( p2(i) );

  end

  for i = 1 : n

    i1 = - p2(i);

    if ( 0 <= i1 )

      i0 = i;

      while ( true )

        i2 = p2(i1);
        p2(i1) = i0;

        if ( i2 < 0 )
          break;
        end

        i0 = i1;
        i1 = i2;

      end

    end

  end

  return
end
