function z = v_polynomial_zeros ( n )

%*****************************************************************************80
%
%% v_polynomial_zeros() returns zeroes of the Chebyshev polynomial V(n,x).
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    26 April 2012
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
  angle = ( (2*n-1):-2:1 ) * pi / ( 2 * n + 1 );
  z = cos ( angle );

  return
end
