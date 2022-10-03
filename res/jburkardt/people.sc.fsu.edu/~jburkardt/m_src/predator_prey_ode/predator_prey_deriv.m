function dydt = predator_prey_deriv ( t, y )

%*****************************************************************************80
%
%% predator_prey_deriv() evaluates the right hand side of predator_prey_ode().
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
%    real T, the current time.
%
%    real Y(2), the current solution variables, rabbits and foxes.
%
%  Output:
%
%    real DYDT(2), the right hand side of the 2 ODE's.
%
  r = y(1);
  f = y(2);

  [ alpha, beta, gamma, delta, t0, y0, tstop ] = predator_prey_parameters ( );

  drdt =   alpha * r - beta  * r * f;
  dfdt = - gamma * f + delta * r * f;

  dydt = [ drdt; dfdt ];

  return
end
