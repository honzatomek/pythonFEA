function value = gegenbauer_integral ( expon, alpha )

%*****************************************************************************80
%
%% gegenbauer_integral() evaluates the integral of a monomial with Gegenbauer weight.
%
%  Discussion:
%
%    The integral:
%
%      Integral ( -1 <= X <= +1 ) x^EXPON (1-x^2)^(ALPHA-1/2) dx
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    03 March 2008
%
%  Author:
%
%    John Burkardt
%
%  Input:
%
%    integer EXPON, the exponent.
%
%    real ALPHA, the exponent of (1-X^2) in the weight factor.
%
%  Output:
%
%    real VALUE, the value of the integral.
%
  if ( mod ( expon, 2 ) == 1 )
    value = 0.0;
    return
  end

  c = expon;

  arg1 = - alpha;
  arg2 =   1.0 + c;
  arg3 =   2.0 + alpha + c;
  arg4 = - 1.0;

  value1 = r8_hyper_2f1 ( arg1, arg2, arg3, arg4 );

  value = 2.0 * gamma ( 1.0 + c ) * gamma ( 1.0 + alpha ) ...
    * value1 / gamma ( 2.0 + alpha  + c );

  return
end
