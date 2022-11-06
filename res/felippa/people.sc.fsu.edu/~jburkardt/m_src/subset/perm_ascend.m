function [ length, sub ] = perm_ascend ( n, p )

%*****************************************************************************80
%
%% perm_ascend() computes the longest ascending subsequence of permutation.
%
%  Discussion:
%
%    Although this routine is intended to be applied to a permutation,
%    it will work just as well for an arbitrary vector.
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
%    integer P(N), the permutation to be examined.
%
%  Output:
%
%    integer LENGTH, the length of the longest increasing subsequence.
%
%    integer SUB(LENGTH), a longest increasing subsequence of A.
%
  top(1:n) = 0;
  top_prev(1:n) = 0;

  length = 0;
  sub = [];

  if ( n <= 0 )
    return
  end

  for i = 1 : n

    k = 0;

    for j = 1 : length
      if ( p(i) <= p(top(j)) )
        k = j;
        break;
      end
    end

    if ( k == 0 )
      length = length + 1;
      k = length;
    end

    top(k) = i;

    if ( 1 < k )
      top_prev(i) = top(k-1);
    else
      top_prev(i) = 0;
    end

  end
%
%  Construct the subsequence.
%
  sub(1:length) = 0;

  j = top(length);
  sub(length) = p(j);

  for i = length-1 : -1 : 1
    j = top_prev(j);
    sub(i) = p(j);
  end

  return
end

