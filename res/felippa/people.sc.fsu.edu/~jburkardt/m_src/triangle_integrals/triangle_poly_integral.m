function q = triangle_poly_integral ( d, p, t )

%*****************************************************************************80
%
%% triangle_poly_integral(): polynomial integral over a triangle.
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
%    real T(2,3), the vertices of the triangle.
%
%  Output:
%
%    real Q, the integral.
%
  m = ( ( d + 1 ) * ( d + 2 ) ) / 2;

  q = 0.0;
  for k = 1 : m
    [ i, j ] = i4_to_pascal ( k );
    qk = triangle_monomial_integral ( i, j, t );
    q = q + p(k) * qk;
  end

  return
end
