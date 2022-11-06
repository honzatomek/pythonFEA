function a = ksub_random ( n, k )

%*****************************************************************************80
%
%% ksub_random() selects a random subset of size K from a set of size N.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    14 June 2004
%
%  Author:
%
%    Original FORTRAN77 version by Albert Nijenhuis, Herbert Wilf.
%    MATLAB version by John Burkardt.
%
%  Reference:
%
%    A Nijenhuis, H Wilf,
%    Combinatorial Algorithms,
%    Academic Press, 1978, second edition,
%    ISBN 0-12-519260-6.
%
%  Input:
%
%    integer N, the size of the set from which subsets are drawn.
%
%    integer K, number of elements in desired subsets.  K must
%    be between 0 and N.
%
%  Output:
%
%    integer A(K).  A(I) is the I-th element of the
%    output set.  The elements of A are in order.
%
  if ( k < 0 )
    fprintf ( 1, '\n' );
    fprintf ( 1, 'KSUB_RANDOM - Fatal error!\n' );
    fprintf ( 1, '  K = %d\n', k );
    fprintf ( 1, '  but 0 <= K is required!\n' );
    error ( 'KSUB_RANDOM - Fatal error!' );
  elseif ( n < k )
    fprintf ( 1, '\n' );
    fprintf ( 1, 'KSUB_RANDOM - Fatal error!\n' );
    fprintf ( 1, '  N = %d\n', n );
    fprintf ( 1, '  K = %d\n', k );
    fprintf ( 1, '  K <= N is required!\n' );
    error ( 'KSUB_RANDOM - Fatal error!' );
  end

  if ( k == 0 )
    return;
  end

  a = zeros ( k, 1 );

  for i = 1 : k
    a(i) = floor ( ( ( i - 1 ) * n ) / k );
  end

  for i = 1 : k

    while ( true )

      ix = randi ( [ 1, n ], 1 );

      l = 1 + floor ( ( ix * k - 1 ) / n );

      if ( a(l) < ix )
        break
      end

    end

    a(l) = a(l) + 1;

  end

  ip = 0;
  is = k;

  for i = 1 : k

    m = a(i);
    a(i) = 0;

    if ( m ~= floor ( ( ( i - 1 ) * n ) / k ) )
      ip = ip + 1;
      a(ip) = m;
    end

  end

  ihi = ip;

  for i = 1 : ihi
    ip = ihi + 1 - i;
    l = 1 + floor ( ( a(ip) * k - 1 ) / n );
    ids = a(ip) - floor ( ( ( l - 1 ) * n ) / k );
    a(ip) = 0;
    a(is) = l;
    is = is - ids;
  end

  for ll = 1 : k

    l = k + 1 - ll;

    if ( a(l) ~= 0 )
      ir = l;
      m0 = 1 + floor ( ( ( a(l) - 1 ) * n ) / k );
      m = floor ( ( a(l) * n ) / k ) - m0 + 1;
    end

    ix = randi ( [ m0, m0+m-1 ], 1 );

    i = l + 1;

    while ( i <= ir )

      if ( ix < a(i) )
        break
      end

      ix = ix + 1;
      a(i-1) = a(i);
      i = i + 1;

    end

    a(i-1) = ix;
    m = m - 1;

  end

  return
end
 
