function b = i4poly_dif ( na, a, d )

%*****************************************************************************80
%
%% i4poly_dif() differentiates an I4POLY.
%
%  Discussion:
%
%    The polynomials are in power sum form.
%
%    The power sum form is:
%
%      p(x) = a(0) + a(1)*x + ... + a(n-1)*x^(n-1) + a(n)*x^(n)
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    21 November 2013
%
%  Author:
%
%    John Burkardt
%
%  Input:
%
%    integer NA, the degree of polynomial A.
%
%    integer A(1:NA+1), the coefficients of a polynomial.
%
%    integer D, the number of times the polynomial
%    is to be differentiated.
%
%  Output:
%
%    integer B(1:NA - D + 1 ), the coefficients of the
%    differentiated polynomial.
%
  if ( na < d )
    b(1) = 0;
    return
  end

  b = zeros ( na - d + 1, 1 );

  for i = 0 : na - d
    b(i+1) = a(i+d+1) * i4_fall ( i + d, d );
  end

  return
end
