function v = w_polynomial_ab ( a, b, m, n, xab )

%*****************************************************************************80
%
%% w_polynomial_ab(): Chebyshev polynomials WAB(N,X) in [A,B].
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
%    integer M, the number of evaluation points.
%
%    integer N, the highest polynomial to compute.
%
%    real XAB(M,1), the evaluation points.
%    It must be the case that A <= XAB(*) <= B.
%
%  Output:
%
%    real V(M,N+1), the values of the Chebyshev polynomials.
%
  x = ( 2.0 * xab - a - b ) / ( b - a );

  v = w_polynomial ( m, n, x );
 
  return
end
