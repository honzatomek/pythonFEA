function value = lf_integral ( n, alpha )

%*****************************************************************************80
%
%% lf_integral() evaluates a monomial integral associated with Lf(n,alpha,x).
%
%  Discussion:
%
%    The integral:
%
%      integral ( 0 <= x < +oo ) x^n * x^alpha * exp ( -x ) dx
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
%    integer N, the exponent.
%    0 <= N.
%
%    real ALPHA, the exponent of X in the weight function.
%
%  Output:
%
%    real VALUE, the value of the integral.
%
  arg = alpha + n + 1;

  exact = gamma ( arg );

  return
end
