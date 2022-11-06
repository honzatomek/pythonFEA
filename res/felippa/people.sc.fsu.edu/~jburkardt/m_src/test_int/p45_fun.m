function fx = p45_fun ( n, x )

%*****************************************************************************80
%
%% p45_fun() evaluates the integrand for problem 45.
%
%  Discussion:
%
%    The problem has a parameter ALPHA that can be set by calling
%    P45_param_set.
%
%    The suggested parameter range is 0.0 <= ALPHA <= 8.0.
%
%  Interval:
%
%    0 <= X <= 1
%
%  Integrand:
%
%    cos ( 2^alpha * x ) / sqrt ( x * ( 1 - x ) )
%
%  Exact Integral:
%
%    pi * cos ( 2^(alpha-1) ) * J0 ( 2^(alpha-1) )
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
  alpha = p45_param_get ( );

  i = find ( x == 0.0 | x == 1.0 );
  j = find ( x ~= 0.0 & x ~= 1.0 );
  fx(i) = 0.0;
  fx(j) = cos ( 2.0^alpha * x(j) ) ./ sqrt ( x(j) .* ( 1.0 - x(j) ) );

  return
end
