function exact = laguerre_integral ( expon )

%*****************************************************************************80
%
%% laguerre_integral() evaluates a monomial Laguerre integral.
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
%    integer EXPON, the exponent.
%    0 <= EXPON.
%
%  Output:
%
%    real EXACT, the value of the integral.
%
  exact = factorial ( expon );

  return
end
