function [ alpha, beta, mu, t0, y0 ] = roessler_parameters ( )

%*****************************************************************************80
%
%% roessler_parameters returns parameters for the roessler ODE.
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
%  Output:
%
%    real ALPHA, BETA, MU: problem parameters.
%
%    real T0: the initial time.
%
%    real Y0[3]: the initial condition at time T0.
%
  alpha = 0.2;
  beta = 0.2;
  mu = 5.7;

  t0 = 0.0;
  y0 = [ 8.0, 1.0, 1.0 ];

  return
end

