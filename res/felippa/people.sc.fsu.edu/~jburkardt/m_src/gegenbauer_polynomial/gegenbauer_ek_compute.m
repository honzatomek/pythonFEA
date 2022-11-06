function [ x, w ] = gegenbauer_ek_compute ( n, alpha )

%*****************************************************************************80
%
%% gegenbauer_ek_compute(): Elhay-Kautsky method for Gauss-Gegenbauer quadrature rule.
%
%  Discussion:
%
%    The integral:
%
%      integral ( -1 <= x <= 1 ) (1-x^2)^(alpha-1/2) * f(x) dx
%
%    The quadrature rule:
%
%      sum ( 1 <= i <= n ) w(i) * f ( x(i) )
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    23 November 2015
%
%  Author:
%
%    Original FORTRAN77 version by Sylvan Elhay, Jaroslav Kautsky.
%    MATLAB version by John Burkardt.
%
%  Reference:
%
%    Sylvan Elhay, Jaroslav Kautsky,
%    Algorithm 655: IQPACK, FORTRAN Subroutines for the Weights of
%    Interpolatory Quadrature,
%    ACM Transactions on Mathematical Software,
%    Volume 13, Number 4, December 1987, pages 399-415.
%
%  Input:
%
%    integer N, the order.
%
%    real ALPHA, determines the exponent of (1-X^2)^(ALPHA-1/2).
%
%  Output:
%
%    real X(N), the abscissas.
%
%    real W(N), the weights.
%

%
%  Define the zero-th moment.
%
  zemu = 2.0 ^ ( 2.0 * alpha + 1.0 ) ...
    * gamma ( alpha + 1.0 ) ...
    * gamma ( alpha + 1.0 ) ...
    / gamma ( 2.0 * alpha + 2.0 );
%
%  Define the Jacobi matrix.
%
  x(1:n) = 0.0;

  bj = zeros ( n, 1 );

  bj(1) = 4.0 * ( alpha + 1.0 ) ^ 2 ...
    / ( ( 2.0 * alpha + 3.0 ) * ( 2.0 * alpha + 2.0 ) ^ 2 );

  for i = 2 : n
    abi = 2.0 * ( alpha + i );
    bj(i) = 4.0 * i * ( alpha + i ) ^ 2 * ( 2.0 * alpha + i ) ...
      / ( ( abi - 1.0 ) * ( abi + 1.0 ) * abi * abi );
  end

  bj(1:n) = sqrt ( bj(1:n) );

  w = zeros ( n, 1 );

  w(1) = sqrt ( zemu );
  w(2:n) = 0.0;
%
%  Diagonalize the Jacobi matrix.
%
  [ x, w ] = imtqlx ( n, x, bj, w );

  w(1:n) = w(1:n) .^ 2;

  return
end
