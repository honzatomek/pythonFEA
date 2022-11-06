function value = j_double_product_integral ( i, j, a, b )

%*****************************************************************************80
%
%% j_double_product_integral(): integral of J(i,x)*J(j,x)*(1-x)^a*(1+x)^b.
%
%  Discussion:
%
%    VALUE = integral ( -1 <= x <= +1 ) J(i,x)*J(j,x)*(1-x)^a*(1+x)^b dx
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
%    real A, B, the parameters.
%    -1 < A, B.
%
%  Output:
%
%    real VALUE, the value of the integral.
%
  if ( i ~= j )
    value = 0.0;
  else
    value = 2^( a + b + 1.0 ) / ( 2 * i + a + b + 1 ) ...
      * gamma ( i + a + 1 ) * gamma ( i + b + 1 ) ...
      / factorial ( i ) / gamma ( i + a + b + 1 );
  end

  return
end
