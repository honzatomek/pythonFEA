function quad_error = simplex_unit_monomial_quadrature ( m, expon, n, x, w )

%*****************************************************************************80
%
%% simplex_unit_monomial_quadrature(): quadrature of monomials in a unit simplex.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    02 March 2017
%
%  Author:
%
%    John Burkardt
%
%  Input:
%
%    integer M, the spatial dimension.
%
%    integer EXPON(M), the exponents.
%
%    integer N, the number of points in the rule.
%
%    real X(M,N), the quadrature points.
%
%    real W(N), the quadrature weights.
%
%  Output:
%
%    real QUAD_ERROR, the quadrature error.
%

%
%  Get the exact value of the integral of the unscaled monomial.
%
  scale = simplex_unit_monomial_integral ( m, expon );
%
%  Evaluate the monomial at the quadrature points.
%
  value = monomial_value ( m, n, expon, x );
%
%  Compute the weighted sum and divide by the exact value.
%
  quad = ( w' * value ) / scale;
%
%  Error:
%
  exact = 1.0;
  quad_error = abs ( quad - exact );

  return
end
