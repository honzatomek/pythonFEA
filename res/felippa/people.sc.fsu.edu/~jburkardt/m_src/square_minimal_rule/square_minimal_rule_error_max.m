function error_max = square_minimal_rule_error_max ( degree )

%*****************************************************************************80
%
%% square_minimal_rule_error_max() returns the maximum error.
%
%  Discussion:
%
%    The rule of given DEGREE should theoretically have zero error
%    for all monomials of degrees 0 <= D <= DEGREE.  This function
%    checks every such monomial and reports the maximum error.
%
%  Licensing:
%
%    This code is distributed under the GNU GPL license.
%
%  Modified:
%
%    19 February 2018
%
%  Author:
%
%    John Burkardt.
%
%  Reference:
%
%    Mattia Festa, Alvise Sommariva,
%    Computing almost minimal formulas on the square,
%    Journal of Computational and Applied Mathematics,
%    Volume 17, Number 236, November 2012, pages 4296-4302.
%
%  Input:
%
%    integer DEGREE, the desired total polynomial degree exactness
%    of the quadrature rule.
%
%  Output:
%
%    real ERROR_MAX, the maximum error observed when using the rule
%    to compute the integrals of all monomials of degree between 0 and DEGREE.
%
  order = square_minimal_rule_order ( degree );
  xyw = square_minimal_rule ( degree );

  x = xyw(:,1);
  y = xyw(:,2);
  w = xyw(:,3);

  error_max = 0.0;

  for d = 0 : degree
    for i = 0 : d
      j = d - i;
      exact = squaresym_monomial_integral ( [ i, j ] );
      s = sum ( w(1:order) .* x(1:order).^i .* y(1:order).^j );
      err = abs ( exact - s );
      error_max = max ( error_max, err );
    end
  end

  return
end
