function z = u_polynomial_zeros ( n )

%*****************************************************************************80
%
%% u_polynomial_zeros() returns zeroes of Chebyshev polynomials U(n,x).
%
%  Discussion:
%
%    The I-th zero of U(n,x) is cos((I-1)*PI/(N-1)), I = 1 to N
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    13 October 2019
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
%    real Z(N), the zeroes of U(n,x).
%
  angle = ( 1:n ) * pi / ( n + 1 );
  z = cos ( angle );

  return
end
