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
%    22 February 2020
%
%  Author:
%
%    John Burkardt
%
%  Reference:
%
%    George Lindfield, John Penny,
%    Numerical Methods Using MATLAB,
%    Second Edition,
%    Prentice Hall, 1999,
%    ISBN: 0-13-012641-1,
%    LC: QA297.P45.
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

  drdt =    2.0 * r - 0.001 * r * f;
  dfdt = - 10.0 * f + 0.002 * r * f;

  drfdt = [ drdt; dfdt ];

  return
end
