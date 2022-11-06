function value = l_integral ( n )

%*****************************************************************************80
%
%% l_integral() evaluates a monomial integral associated with L(n,x).
%
%  Discussion:
%
%    The integral:
%
%      integral ( 0 <= x < +oo ) x^n * exp ( -x ) dx
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    12 January 2021
%
%  Author:
%
%    John Burkardt
%
%  Input:
%
%    integer N, the exponent.
%    0 <= N.
%
%  Output:
%
%    real VALUE, the value of the integral.
%
  value = factorial ( n );

  return
end
