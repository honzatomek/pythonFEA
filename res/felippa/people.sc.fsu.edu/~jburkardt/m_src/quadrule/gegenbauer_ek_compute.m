function [ x, w ] = gegenbauer_ek_compute ( n, alpha, a, b )

%*****************************************************************************80
%
%% gegenbauer_ek_compute() computes a Gauss-Gegenbauer quadrature.
%
%  Discussion:
%
%    The integral:
%
%      Integral ( A <= X <= B ) (1-X^2)^ALPHA * F(X) dX
%
%    The quadrature rule:
%
%      Sum ( 1 <= I <= N ) W(I) * F ( X(I) )
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    19 November 2015
%
%  Author:
%
%    John Burkardt.
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
%    real ALPHA, the exponent of (1-X^2) in the weight.  
%    -1.0 < ALPHA is required.
%
%    real A, B, the left and right endpoints 
%    of the interval.
%
%  Output:
%
%    real X(N), the abscissas.
%
%    real W(N), the weights.
%

%
%  Check N.
%
  if ( n < 1 )
    fprintf ( 1, '\n' );
    fprintf ( 1, 'GEGENBAUER_EK_COMPUTE - Fatal error!\n' );
    fprintf ( 1, '  1 <= N is required.\n' );
    error ( 'GEGENBAUER_EK_COMPUTE - Fatal error!' );
  end
%
%  Check ALPHA.
%
  if ( alpha <= -1.0 )
    fprintf ( 1, '\n' );
    fprintf ( 1, 'GEGENBAUER_EK_COMPUTE - Fatal error!\n' );
    fprintf ( 1, '  -1.0 < ALPHA is required.\n' );
    error ( 'GEGENBAUER_EK_COMPUTE - Fatal error!' );
  end
%
%  Call the general Gauss quadrature routine to get X and W.
%
  kind = 3;
  beta = 0.0;

  [ x, w ] = cgqf ( n, kind, alpha, beta, a, b );

  return
end
