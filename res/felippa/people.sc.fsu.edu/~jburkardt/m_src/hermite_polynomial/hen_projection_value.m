function v = hen_projection_value ( n, c, m, x )

%*****************************************************************************80
%
%% hen_projection_value(): evaluation projection onto Hen(i,x).
%
%  Discussion:
%
%    Hen(i,x) is the normalized probabilist's Hermite polynomial of degree I.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    18 March 2012
%
%  Author:
%
%    John Burkardt
%
%  Input:
%
%    integer N, the highest order polynomial.
%
%    real C(N+1), the projection coefficients.
%
%    real M, the number of evaluation points.
%
%    real X(M), the evaluation points.
%
%  Output:
%
%    real V(M), the value of the projection function.
%
  phi = hen_polynomial_value ( m, n, x );

  v = phi * c;

  return
end
