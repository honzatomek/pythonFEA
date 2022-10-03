function [ alpha, beta, gamma, delta ] = predator_prey_parameters ( )

%*****************************************************************************80
%
%% predator_prey_parameters returns predator prey parameters.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    29 October 2020
%
%  Author:
%
%    John Burkardt
%
%  Output:
%
%    real ALPHA, BETA, GAMMA, DELTA: the parameter values;
%
  alpha = 2.0;
  beta = 0.001;
  gamma = 10.0;
  delta = 0.002;

  return
end

