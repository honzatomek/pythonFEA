function p_inv = perm1_inverse2 ( n, p )

%*****************************************************************************80
%
%% perm1_inverse2() inverts a permutation of (1,...,N).
%
%  Discussion:
%
%    The routine needs no extra vector storage in order to compute the
%    inverse of a permutation.
%
%    This feature might be useful if the permutation is large.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license. 
%
%  Modified:
%
%    12 June 2015
%
%  Author:
%
%    John Burkardt.
%
%  Input:
%
%    integer N, the number of objects in the permutation.
%
%    integer P(N), the permutation.
%
%  Output:
%
%    integer P_INV(N), the inverse permutation.
%
  ierror = perm1_check ( n, p );

  if ( ierror )
    fprintf ( 1, '\n' );
    fprintf ( 1, 'PERM1_INVERSE2 - Fatal error!\n' );
    fprintf ( 1, '  The input array does not represent\n' );
    fprintf ( 1, '  a proper permutation.  In particular, the\n' );
    fprintf ( 1, '  array is missing the value %d\n', ierror );
    error ( 'PERM1_INVERSE2 - Fatal error!' );
  end

  p_inv(1:n) = p(1:n);

  for m = n : -1 : 1

    i = p_inv(m);

    if ( i < 0 )

      p_inv(m) = -i;

    elseif ( i ~= m )

      k = m;

      while ( true )

        j = p_inv(i);
        p_inv(i) = -k;

        if ( j == m )
          p_inv(m) = i;
          break
        end

        k = i;
        i = j;

      end

    end

  end

  return
end
