function drfdt = predator_prey_deriv ( t, rf )

%*****************************************************************************80
%
%% predator_prey_deriv evaluates the right hand side of the system.
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
%    real RF(2), the current solution variables, rabbits and foxes.
%
%  Output:
%
%    real DRFDT(2), the right hand side of the 2 ODE's.
%
  r = rf(1);
  f = rf(2);

  [ alpha, beta, gamma, delta ] = predator_prey_parameters ( );

  drdt =   alpha * r - beta  * r * f;
  dfdt = - gamma * f + delta * r * f;

  drfdt = [ drdt; dfdt ];

  return
end
