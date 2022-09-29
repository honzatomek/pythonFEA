function [ g, m1, m2, l1, l2, t0, y0 ] = double_pendulum_parameters ( )

%*****************************************************************************80
%
%% double_pendulum_parameters: parameters for the double pendulum ODE.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    11 November 2020
%
%  Author:
%
%    John Burkardt
%
%  Input:
%
%    real G: the gravitational force coefficient.
%
%    real M1, M2: the masses of pendulums 1 and 2.
%
%    real L1, L2: the lengths of pendulums 1 and 2.
%
%    real T0: the initial time.
%
%    real Y0[4]: the initial condition.
%
  g = 9.81;
  m1 = 1.0;
  m2 = 1.0;
  l1 = 1.0;
  l2 = 1.0;

  t0 = 0.0;
  y0 = [ 0.25; 0.0; 0.0; 0.0 ];

  return
end

