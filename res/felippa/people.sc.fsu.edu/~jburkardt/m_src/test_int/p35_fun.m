function fx = p35_fun ( n, x )

%*****************************************************************************80
%
%% p35_fun() evaluates the integrand for problem 35.
%
%  Interval:
%
%    -9 <= x <= 100
%
%  Integrand:
%
%    1 / sqrt ( abs ( x ) )
%
%  Exact Integral:
%
%    26
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
%    Hermann Engels,
%    Numerical Quadrature and Cubature,
%    Academic Press, 1980.
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
  fx = 1.0 ./ sqrt ( abs ( x ) );

  return
end
