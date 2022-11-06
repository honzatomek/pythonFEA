function q = triangle01_poly_integral ( d, p )

%*****************************************************************************80
%
%% triangle01_poly_integral(): polynomial integral over the unit triangle.
%
%  Discussion:
%
%    The unit triangle is T = ( (0,0), (1,0), (0,1) ).
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license. 
%
%  Modified:
%
%    18 April 2015
%
%  Author:
%
%    John Burkardt
%
%  Input:
%
%    integer D, the degree of the polynomial.
%
%    real P(M), the polynomial coefficients.
%    M = ( ( D + 1 ) * ( D + 2 ) ) / 2.
%
%  Output:
%
%    real Q, the integral.
%
  m = ( ( d + 1 ) * ( d + 2 ) ) / 2;

  q = 0.0;
  for k = 1 : m
    [ i, j ] = i4_to_pascal ( k );
    qk = triangle01_monomial_integral ( i, j );
    q = q + p(k) * qk;
  end

  return
end
