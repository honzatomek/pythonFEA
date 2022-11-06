function value = he_double_product_integral ( i, j )

%*****************************************************************************80
%
%% he_double_product_integral(): integral of He(i,x)*He(j,x)*e^(-x^2/2).
%
%  Discussion:
%
%    He(i,x) represents the probabilist's Hermite polynomial.
%
%    VALUE = integral ( -oo < x < +oo ) H(i,x)*H(j,x) exp(-x^2/2) dx
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    12 January 2021
%
%  Author:
%
%    John Burkardt
%
%  Input:
%
%    integer I, J, the polynomial indices.
%
%  Output:
%
%    real VALUE, the value of the integral.
%
  if ( i ~= j )
    value = 0.0;
  else
    value = factorial ( i );
  end

  return
end
