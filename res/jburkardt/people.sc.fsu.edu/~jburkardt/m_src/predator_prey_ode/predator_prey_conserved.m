function h = predator_prey_conserved ( y )

%*****************************************************************************80
%
%% predator_prey_conserved() evaluates a conserved quantity for predator_prey_ode().
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    17 December 2021
%
%  Author:
%
%    John Burkardt
%
%  Input:
%
%    real Y(N,2): the current solution variables.
%
%  Output:
%
%    real H(N): the value of the conserved quantity.
%
  [ alpha, beta, gamma, delta, t0, y0, tstop ] = predator_prey_parameters ( );

  y_equi = predator_prey_equilibrium ( );
  h_equi = delta * y_equi(1) - gamma * log ( y_equi(1) ) ...
    + beta * y_equi(2) - alpha * log ( y_equi(2) );

  r = y(:,1);
  f = y(:,2);

  h = delta * r - gamma * log ( r ) + beta * f - alpha * log ( f ) - h_equi;

  return
end

