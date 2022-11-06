function v = w_polynomial_ab_value ( a, b, n, xab )

%*****************************************************************************80
%
%% w_polynomial_ab_value(): Chebyshev polynomial WAB(n,x) in [A,B]
%
%  Discussion:
%
%    WAB(n,x) = W(n,(2*x-a-b)/(b-a))
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    20 July 2015
%
%  Author:
%
%    John Burkardt
%
%  Input:
%
%    real A, B, the domain of definition.
%
%    integer N, the polynomial degree.
%
%    real XAB, the evaluation point.
%    A <= XAB <= B.
%
%  Output:
%
%    real V, the value of WAB(N,XAB).
%
  x = ( 2.0 * xab - a - b ) / ( b - a );

  v = w_polynomial_value ( n, x );
 
  return
end
