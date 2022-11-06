function value = hermite_integral ( n )

%*****************************************************************************80
%
%% hermite_integral() evaluates a monomial Hermite integral.
%
%  Discussion:
%
%    The integral:
%
%      Integral ( -oo < x < +oo ) x^n exp(-x^2) dx
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    19 February 2008
%
%  Author:
%
%    John Burkardt
%
%  Input:
%
%    integer N, the order of the integral.
%    0 <= N.
%
%  Output:
%
%    real VALUE, the value of the integral.
%
  if ( n < 0 )

    value = - Inf;

  elseif ( mod ( n, 2 ) == 1 )

    value = 0.0;

  else

    value = r8_factorial2 ( n - 1 ) * sqrt ( pi ) / 2.0^( n / 2 );

  end

  return
end
