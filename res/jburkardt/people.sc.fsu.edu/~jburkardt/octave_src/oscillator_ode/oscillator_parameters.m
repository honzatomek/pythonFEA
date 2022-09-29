function [ alpha, beta, epsilon, t0, y0 ] = oscillator_parameters ( )

%*****************************************************************************80
%
%% oscillator_parameters returns oscillator parameters.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    10 November 2020
%
%  Author:
%
%    John Burkardt
%
%  Output:
%
%    real ALPHA: the initial value of Y'
%
%    real BETA: the initial value of Y/EPSILON
%
%    real EPSILON: the period / ( 2*pi).
%
%    real T0: the initial time.
%
%    real Y0(2): the solution values at time t0.
%
  alpha = 0.0;
  beta = 1.0;
  epsilon = 0.1;

  t0 = 0.0;
  y0 = [ beta * epsilon, alpha ];

  return
end

