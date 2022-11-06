function fx = p07_fun ( n, x )

%*****************************************************************************80
%
%% p07_fun() evaluates the integrand for problem 7.
%
%  Discussion:
%
%    The integrand is singular at x = 0.
%
%  Interval:
%
%    0 <= x <= 1
%
%  Integrand:
%
%    1 / sqrt ( x )
%
%  Antiderivative:
%
%    2 * sqrt ( x )
%
%  Exact Integral:
%
%    2
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    14 November 2009
%
%  Author:
%
%    John Burkardt
%
%  Reference:
%
%    David Kahaner,
%    Comparison of Numerical Quadrature Formulas,
%    in Mathematical Software, edited by John R Rice,
%    Academic Press, 1971
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
  fx(j) = 1.0 ./ sqrt ( x(j) );

  return
end
