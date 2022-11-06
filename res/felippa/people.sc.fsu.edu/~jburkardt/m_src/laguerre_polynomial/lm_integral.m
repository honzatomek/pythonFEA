function value = lm_integral ( n, m )

%*****************************************************************************80
%
%% lm_integral() evaluates a monomial integral associated with Lm(n,m,x).
%
%  Discussion:
%
%    The integral:
%
%      integral ( 0 <= x < +oo ) x^n * x^m * exp ( -x ) dx
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
%    Input, integer N, the exponent.
%    0 <= N.
%
%    Input, integer M, the parameter.
%    0 <= M.
%
%  Output:
%
%    real VALUE, the value of the integral.
%
  value = factorial ( n + m );

  return
end
