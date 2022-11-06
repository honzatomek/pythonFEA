function v = bernstein_vandermonde ( n )

%*****************************************************************************80
%
%% bernstein_vandermonde() returns the Bernstein Vandermonde matrix.
%
%  Discussion:
%
%    The Bernstein Vandermonde matrix of order N is constructed by
%    evaluating the N Bernstein polynomials of degree N-1 at N equally
%    spaced points between 0 and 1.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    03 December 2015
%
%  Author:
%
%    John Burkardt
%
%  Input:
%
%    integer N, the order of the matrix.
%
%  Output:
%
%    real A(N,N), the Bernstein Vandermonde matrix.
%
  if ( n == 1 )
    v = ones ( 1, 1 );
    return
  end

  v = zeros ( n, n );
  x = linspace ( 0.0, 1.0, n );

  for i = 1 : n
    v(i,1:n) = bernstein_poly_01 ( n - 1, x(i) );
  end

  return
end

