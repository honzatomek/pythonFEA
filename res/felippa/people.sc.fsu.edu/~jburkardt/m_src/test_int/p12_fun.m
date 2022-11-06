function fx = p12_fun ( n, x )

%*****************************************************************************80
%
%% p12_fun() evaluates the integrand for problem 12.
%
%  Discussion:
%
%    The integrand has a removable singularity at X = 0.
%
%  Interval:
%
%    0 <= x <= 1
%
%  Integrand:
%
%    x / ( exp ( x) - 1 )
%
%  Antiderivative:
%
%    The Debye function.
%
%  Approximate Integral (20 digits):
%
%    0.77750463411224827642...
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    12 November 2009
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
%    Academic Press, 1971.
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
  fx(i) = 1.0;
  j = find ( x ~= 0.0 );
  fx(j) = x(j) ./ ( exp ( x(j) ) - 1.0 );

  return
end
