function fx = p36_fun ( n, x )

%*****************************************************************************80
%
%% p36_fun() evaluates the integrand for problem 49.
%
%  Discussion:
%
%    The problem has a parameter ALPHA that can be set by calling
%    P36_param_set.  It had a default value of -0.9.
%
%    The integrand has an endpoint singularity at X=0.
%
%    Suggested values of ALPHA include -0.9 through 2.6.
%
%  Interval:
%
%    0 <= x <= 1
%
%  Integrand:
%
%    x^alpha * ln ( 1 / x )
%
%  Exact Integral:
%
%    1 / ( alpha + 1 )^2
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
%    Robert Piessens, Elise de Doncker-Kapenga,
%    Christian Ueberhuber, David Kahaner,
%    QUADPACK: A Subroutine Package for Automatic Integration,
%    Springer, 1983, page 83.
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
  alpha = p36_param_get ( );

  i = find ( x == 0.0 );
  fx(i) = 0.0;
  j = find ( x ~= 0.0 );
  fx(j) = x(j).^alpha .* log ( 1.0 ./ x(j) );

  return
end
