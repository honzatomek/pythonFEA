function value = r8vec_lt ( n, a1, a2 )

%*****************************************************************************80
%
%% r8vec_lt() == ( A1 < A2 ) for R8VEC's.
%
%  Discussion:
%
%    The comparison is lexicographic.
%
%    A1 < A2  <=>                              A1(1) < A2(1) or
%                 ( A1(1)     == A2(1)     and A1(2) < A2(2) ) or
%                 ...
%                 ( A1(1:N-1) == A2(1:N-1) and A1(N) < A2(N)
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    05 February 2005
%
%  Author:
%
%    John Burkardt
%
%  Input:
%
%    integer N, the dimension of the vectors.
%
%    real A1(N), A2(N), the vectors to be compared.
%
%  Output:
%
%    logical VALUE, is true if and only if A1 < A2.
%
  value = false;

  for i = 1 : n

    if ( a1(i) < a2(i) )
      value = true;
      break
    elseif ( a2(i) < a1(i) )
      value = false;
      break
    end

  end

  return
end
