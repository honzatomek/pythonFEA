function exact = chebyshev3_integral ( expon )

%*****************************************************************************80
%
%% chebyshev3_integral() evaluates a monomial Chebyshev type 3 integral.
%
%  Discussion:
%
%    The integral:
%
%      integral ( -1 <= x <= +1 ) x^n / sqrt ( 1 - x^2 ) dx
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
%  Output:
%
%    real EXACT, the value of the exact integral.
%

%
%  Get the exact value of the integral.
%
  if ( mod ( expon, 2 ) == 0 )

    top = 1;
    bot = 1;
    for i = 2 : 2 : expon
      top = top * ( i - 1 );
      bot = bot *   i;
    end
	
    exact = pi * top / bot;

  else

    exact = 0.0;
	
  end

  return
end
