function fx = p43_fun ( n, x )

%*****************************************************************************80
%
%% p43_fun() evaluates the integrand for problem 43.
%
%  Discussion:
%
%    The problem has a parameter ALPHA that can be set by calling
%    P43_param_set.
%
%    The suggested parameter range is 0.1 <= ALPHA <= 2.0.
%
%    The integrand has an algebraic endpoint singularity at X = 1
%    times a singular factor.
%
%  Interval:
%
%    0 <= x <= 1
%
%  Integrand:
%
%    ( ln ( 1 / x ) )^( alpha - 1 )
%
%  Exact Integral:
%
%    Gamma(alpha)
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
%    Robert Piessens, Elise de Doncker-Kapenga,
%    Christian Ueberhuber, David Kahaner,
%    QUADPACK: A Subroutine Package for Automatic Integration,
%    Springer, 1983, page 84.
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
  alpha = p43_param_get ( );

  i = find ( x == 0.0 | x == 1.0 );
  fx(i) = 0.0;
  j = find ( x ~= 0.0 & x ~= 1.0 );
  fx(j) = ( log ( 1.0 ./ x(j) ) ).^( alpha - 1.0 );

  return
end
