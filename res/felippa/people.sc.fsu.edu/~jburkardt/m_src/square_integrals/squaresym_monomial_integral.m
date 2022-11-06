function integral = squaresym_monomial_integral ( e )

%*****************************************************************************80
%
%% squaresym_monomial_integral(): integrals over the symmetric square in 2D.
%
%  Discussion:
%
%    The integration region is 
%
%      -1 <= X <= 1,
%      -1 <= Y <= 1.
%
%    The monomial is F(X,Y) = X^E(1) * Y^E(2).
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license. 
%
%  Modified:
%
%    19 February 2019
%
%  Author:
%
%    John Burkardt
%
%  Reference:
%
%    Philip Davis, Philip Rabinowitz,
%    Methods of Numerical Integration,
%    Second Edition,
%    Academic Press, 1984, page 263.
%
%  Input:
%
%    integer E(2), the exponents.  Each exponent must be nonnegative.
%
%  Output:
%
%    real INTEGRAL, the integral.
%
  m = 2;

  if ( any ( e(1:m) < 0 ) )
    fprintf ( 1, '\n' );
    fprintf ( 1, 'SQUARESYM_MONOMIAL_INTEGRAL - Fatal error!\n' );
    fprintf ( 1, '  All exponents must be nonnegative.\n' );
    error ( 'SQUARESYM_MONOMIAL_INTEGRAL - Fatal error!' );
  end

  if ( any ( mod ( e(1:m), 2 ) == 1 ) )
    integral = 0.0;
  else
    integral = 4.0 / ( e(1) + 1 ) / ( e(2) + 1 );
  end

  return
end
