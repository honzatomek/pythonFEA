function z = t_polynomial_zeros ( n )

%*****************************************************************************80
%
%% t_polynomial_zeros() returns zeroes of the Chebyshev polynomial T(n,x).
%
%  Discussion:
%
%    The I-th zero is cos((2*I-1)*PI/(2*N)), I = 1 to N
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
%    real Z(N), the zeroes.
%
  angle = (1:2:(2*n-1)) * pi / 2.0 / n;
  z = cos ( angle );

  return
end
