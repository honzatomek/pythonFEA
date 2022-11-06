function c = hen_projection_data ( m, n, x, d )

%*****************************************************************************80
%
%% hen_projection_data(): project data onto Hen(0:n,x).
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
%    27 March 2012
%
%  Author:
%
%    John Burkardt
%
%  Input:
%
%    integer M, the number of data values.
%
%    integer N, the degree of the highest Hermite polynomial.
%    0 <= N.
%
%    real X(M,1), the data abscissas.  These need not be sorted.
%
%    real D(M,1), the data values.
%
%  Output:
%
%    real C(N+1,1), the approximate projection coefficients.
%
  x = x(:);
  d = d(:);
%
%  Compute the M by N+1 Hermite Vandermonde matrix.
%
  v = hen_polynomial_value ( m, n, x );
%
%  Compute the least-squares solution.
%
  c = v \ d;

  return
end
