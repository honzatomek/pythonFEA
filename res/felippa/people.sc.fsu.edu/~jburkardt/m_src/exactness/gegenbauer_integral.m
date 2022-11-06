function s = gegenbauer_integral ( p, lambda )

%*****************************************************************************80
%
%% gegenbauer_integral() evaluates a monomial integral with Gegenbauer weight.
%
%  Discussion:
%
%    The integral:
%
%      integral ( -1 <= x < +1 ) x^p * ( 1 - x^2 )^(lambda-1/2) dx
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    10 January 2016
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
%    real LAMBDA, the exponent term.
%    -1/2 < LAMBDA.
%
%  Output:
%
%    real S, the value of the integral.
%
  if ( mod ( p, 2 ) == 0 )
    s = gamma ( p / 2.0 + 0.5 ) * gamma ( lambda + 0.5 ) ...
      / gamma ( p / 2.0 + lambda + 1.0 );
  else
    s = 0.0;
  end

  return
end

