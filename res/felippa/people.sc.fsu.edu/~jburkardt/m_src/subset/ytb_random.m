function a = ytb_random ( n, lambda )

%*****************************************************************************80
%
%% ytb_random() selects a random Young tableau of a given shape.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    18 June 2004
%
%  Author:
%
%    MATLAB version by John Burkardt.
%
%  Reference:
%
%    Albert Nijenhuis, Herbert Wilf,
%    Combinatorial Algorithms,
%    Academic Press, 1978, second edition,
%    ISBN 0-12-519260-6.
%
%  Input:
%
%    integer N, the integer which has been partitioned.
%
%    integer LAMBDA(N), the partition of N.
%    N = sum ( 1 <= I <= N ) LAMBDA(I).
%
%  Output:
%
%    integer A(N), the vector describing the Young tableau.
%
  a = zeros ( n, 1 );

  i = 0;
  k = 0;

  while ( true )

    i = i + 1;
    for j = 1 : lambda(i)
      a(j) = a(j) + 1;
      k = k + 1;
    end

    if ( n <= k )
      break;
    end

  end

  for m = 1 : n

    while ( true )

      i = randi ( [ 1, a(1) ], 1 );
      j = randi ( [ 1, lambda(1) ], 1 );

      if ( i <= a(j) && j <= lambda(i) )
        break;
      end

    end

    while ( true )

      ih = a(j) + lambda(i) - i - j;

      if ( ih == 0 )
        break;
      end

      k = randi ( [ 1, ih ], 1 );

      if ( k <= lambda(i)-j )
        j = j + k;
      else
        i = k - lambda(i) + i + j;
      end

    end

    lambda(i) = lambda(i) - 1;
    a(j) = a(j) - 1;
    a(n+1-m) = i;

  end

  for i = 1 : n
    lambda(a(i)) = lambda(a(i)) + 1;
  end

  return
end
