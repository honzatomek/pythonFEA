function value = i4poly_to_i4 ( n, a, x )

%*****************************************************************************80
%
%% i4poly_to_i4() evaluates an integer polynomial.
%
%  Discussion:
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
%    05 July 2004
%
%  Author:
%
%    John Burkardt
%
%  Input:
%
%    integer N, the degree of the polynomial.
%
%    integer A(1:N+1), the polynomial coefficients.
%    A(1) is the constant term and
%    A(N+1) is the coefficient of X**N.
%
%    integer X, the point at which the polynomial is to be evaluated.
%
%  Output:
%
%    integer VALUE, the value of the polynomial.
%
  value = 0;

  for i = n : -1 : 0
    value = value * x + a(i+1);
  end

  return
end
