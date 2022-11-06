function [ alpha, beta, gamma ] = tribonacci_roots ( )

%*****************************************************************************80
%
%% tribonacci_roots() returns the Tribonacci roots.
%
%  Discussion:
%
%    The Nth Tribonacci number is defined by:
%      T(N) = T(N-1) + T(N-2) + T(N-3)
%    with
%      T(1) = 0, T(2) = 0, T(3) = 1.
%
%    The related polynomial equation
%      x^3 - x^2 - x - 1 = 0
%
%     ALPHA, BETA, and GAMMA are the roots of this equation.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    09 May 2021
%
%  Author:
%
%    John Burkardt
%
%  Reference:
%
%    W R Spickerman,
%    Binet's formula for the Tribonacci sequence,
%    Fibonacci Quarterly, 
%    Volume 20, Number 2, pages 118-120, May 1982.
%
%  Output:
%
%    real ALPHA, complex BETA, complex GAMMA, the roots.
%
  rho = nthroot ( 19.0 + 3.0 * sqrt ( 33.0 ), 3 );
  tau = nthroot ( 19.0 - 3.0 * sqrt ( 33.0 ), 3 );

  a = 2.0 - rho - tau;
  b = sqrt ( 3.0 ) * ( rho - tau );

  alpha = ( 1.0 + rho + tau ) / 3.0;
  beta =  ( a + b * i ) / 6.0;
  gamma = ( a - b * i ) / 6.0;

  return
end

