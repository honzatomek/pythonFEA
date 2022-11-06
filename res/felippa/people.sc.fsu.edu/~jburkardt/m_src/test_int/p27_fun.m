function fx = p27_fun ( n, x )

%*****************************************************************************80
%
%% p27_fun() evaluates the integrand for problem 27.
%
%  Interval:
%
%    0 <= x <= 1
%
%  Integrand:
%
%    1 / ( x^(1/2) + x^(1/3) )
%
%  Exact Integral:
%
%    5 - 6 * ln ( 2 )
%
%  Approximate Integral (20 digits):
%
%    0.84111691664032814350...
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    04 November 2009
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
%    Dover, 2007,
%    ISBN: 0486453391,
%    LC: QA299.3.D28.
%
%  Input:
%
%    integer N, the number of evaluation points.
%
%    real X(N), the evaluation points.
%
%  Output:
%
%    real FX(N), the integrand values.
%
  i = find ( x == 0.0 );
  fx(i) = 0.0;
  j = find ( x ~= 0.0 );
  fx(j) = 1.0 ./ ( sqrt ( x(j) ) + x(j).^(1.0/3.0) );

  return
end
