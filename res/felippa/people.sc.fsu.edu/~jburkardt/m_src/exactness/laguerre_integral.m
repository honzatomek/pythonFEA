function s = laguerre_integral ( p )

%*****************************************************************************80
%
%% laguerre_integral() evaluates a monomial integral associated with L(n,x).
%
%  Discussion:
%
%    The integral:
%
%      integral ( 0 <= x < +oo ) x^p * exp ( -x ) dx
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
%    integer P, the exponent.
%    0 <= P.
%
%  Output:
%
%    real S, the value of the integral.
%
  s = factorial ( p );

  return
end

