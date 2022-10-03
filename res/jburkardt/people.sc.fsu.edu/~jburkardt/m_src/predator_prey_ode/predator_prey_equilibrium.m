function [ y_equi ] = predator_prey_equilibrium ( )

%*****************************************************************************80
%
%% predator_prey_equilibrium() returns the equilibrium for predator_prey_ode().
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
%  Output:
%
%    real Y_EQUI(2): the equilibrium solution.
%
  [ alpha, beta, gamma, delta, t0, y0, tstop ] = predator_prey_parameters ( );

  y_equi = [ gamma/delta, alpha/beta ];

  return
end

