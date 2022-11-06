function value = i4vec_lcm ( n, v )

%*****************************************************************************80
%
%% i4vec_lcm() returns the least common multiple of an I4VEC.
%
%  Discussion:
%
%    An I4VEC is a vector of I4's.
%
%    The value LCM returned has the property that it is the smallest integer
%    which is evenly divisible by every element of V.
%
%    The entries in V may be negative.
%
%    If any entry of V is 0, then LCM is 0.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    02 July 2009
%
%  Author:
%
%    John Burkardt
%
%  Input:
%
%    integer N, the order of V.
%
%    integer V(N), the vector.
%
%  Output:
%
%    integer VALUE, the least common multiple of V.
%
  value = 1;

  for i = 1 : n

    if ( v(i) == 0 )
      value = 0;
      return
    end

    value = lcm ( value, v(i) );

  end

  return
end
