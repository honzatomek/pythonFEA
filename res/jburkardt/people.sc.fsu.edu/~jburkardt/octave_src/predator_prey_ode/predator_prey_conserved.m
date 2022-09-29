function h = predator_prey_conserved ( rf )

%*****************************************************************************80
%
%% predator_prey_conserved evaluates a conserved quantity.
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
%  Input:
%
%    real RF(N,2): the current solution variables, rabbits and foxes.
%
%  Output:
%
%    real H(N): the value of the conserved quantity.
%
  r = rf(:,1);
  f = rf(:,2);

  [ alpha, beta, gamma, delta ] = predator_prey_parameters ( );

  h = delta * r - gamma * log ( r ) + beta * f - alpha * log ( f );

  return
end

