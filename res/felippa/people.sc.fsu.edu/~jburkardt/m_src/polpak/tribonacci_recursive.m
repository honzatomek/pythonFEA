function f = tribonacci_recursive ( n )

%*****************************************************************************80
%
%% tribonacci_recursive() computes the first N Tribonacci numbers.
%
%  First terms:
%
%      0
%      0
%      1
%      1
%      2
%      4
%      7
%     13
%     24     
%     44
%     81
%    149
%    274
%    504
%
%  Recursion:
%
%    T(1) = 0
%    T(2) = 0
%    T(3) = 1
%
%    T(N) = T(N-1) + T(N-2) + T(N-3)
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    09 May 2021
%
%  Author:
%
%    John Burkardt
%
%  Input:
%
%    integer N, the number of values to compute.
%
%  Output:
%
%    integer F(N), the first N Tribonacci numbers.
%
  if ( n <= 0 )
    f = [];
    return
  end

  f(1) = 0;

  if ( n <= 1 )
    return
  end

  f(2) = 0;
  
  if ( n <= 2 )
    return
  end

  f(3) = 1;
  
  for i = 4 : n
    f(i) = sum ( f(i-3:i-1) );
  end

  return
end
