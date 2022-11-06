function integral = disk01_quarter_monomial_integral ( e )

%*****************************************************************************80
%
%% disk01_quarter_monomial_integral(): monomial integrals in unit quarter disk.
%
%  Discussion:
%
%    The integration region is 
%
%      X^2 + Y^2 <= 1.
%      0 <= X, 0 <= Y.
%
%    The monomial is F(X,Y) = X^E(1) * Y^E(2).
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license. 
%
%  Modified:
%
%    05 May 2016
%
%  Author:
%
%    John Burkardt
%
%  Input:
%
%    integer E(2), the exponents of X and Y in the 
%    monomial.  Each exponent must be nonnegative.
%
%  Output:
%
%    real INTEGRAL, the integral.
%
  f1 = gamma ( ( e(1)        + 3 ) / 2.0 );
  f2 = gamma ( (        e(2) + 1 ) / 2.0 );
  f3 = gamma ( ( e(1) + e(2) + 4 ) / 2.0 );

  integral = f1 * f2 / f3 / 2.0 / ( 1.0 + e(1) );

  return
end
