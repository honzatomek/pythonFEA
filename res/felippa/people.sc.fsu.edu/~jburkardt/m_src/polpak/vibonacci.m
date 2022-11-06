function v = vibonacci ( n )

%*****************************************************************************80
%
%% vibonacci() computes the first N Vibonacci numbers.
%
%  Discussion:
%
%    The "Vibonacci numbers" are a generalization of the Fibonacci numbers:
%      V(N+1) = +/- V(N) +/- V(N-1)
%    where the signs are chosen randomly.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    07 March 2005
%
%  Author:
%
%    John Burkardt
%
%  Reference:
%
%    Brian Hayes,
%    The Vibonacci Numbers,
%    American Scientist,
%    July-August 1999, Volume 87, Number 4.
%
%    Divakar Viswanath,
%    Random Fibonacci sequences and the number 1.13198824,
%    Mathematics of Computation, 1998.
%
%  Input:
%
%    integer N, the highest number to compute.
%
%  Output:
%
%    integer V(N), the first N Vibonacci numbers.  By convention,
%    V(1) and V(2) are taken to be 1.
%
  if ( n <= 0 )
    v = [];
    return
  end

  v(1) = 1;

  if ( n <= 1 )
    return
  end

  v(2) = 1;

  for i = 3 : n
    
    j = randi ( [ 0, 1 ], 1 );

    if ( j == 0 )
      s1 = -1;
    else
      s1 = +1;
    end

    j = randi ( [ 0, 1 ], 1 );

    if ( j == 0 )
      s2 = -1;
    else
      s2 = +1;
    end

    v(i) = s1 * v(i-1) + s2 * v(i-2);

  end
 
  return
end

