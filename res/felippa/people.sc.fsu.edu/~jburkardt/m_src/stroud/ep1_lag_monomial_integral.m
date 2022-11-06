function value = ep1_lag_monomial_integral ( expon )

%*****************************************************************************80
%
%% ep1_lag_monomial_integral(): integral of monomial with Laguerre weight on EP1.
%
%  Discussion:
%
%    EP1 is the interval [0,+oo) with exponential or Laguerre weight function:
%
%      w(x) = exp ( - x )
%
%    value = integral ( 0 <= x < oo ) x^expon exp ( - x ) dx
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    11 January 2021
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
%    real VALUE, the value of the integral.
%
  value = factorial ( expon );

  return
end
