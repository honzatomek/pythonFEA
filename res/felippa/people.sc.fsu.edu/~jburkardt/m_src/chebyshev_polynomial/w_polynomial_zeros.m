function z = w_polynomial_zeros ( n )

%*****************************************************************************80
%
%% w_polynomial_zeros() returns zeroes of the Chebyshev polynomial W(n,x).
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    13 July 2015
%
%  Author:
%
%    John Burkardt
%
%  Input:
%
%    integer N, the order of the polynomial.
%
%  Output:
%
%    real Z(N), the zeroes.
%
  angle = ( 2*n:-2:2 ) * pi / ( 2 * n + 1 );
  z = cos ( angle );

  return
end
