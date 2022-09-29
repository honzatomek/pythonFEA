function h = pendulum_conserved ( y )

%*****************************************************************************80
%
%% pendulum_conserved returns a conserved quantity for the pendulum ODE.
%
%  Discussion:
%
%    This conserved quantity can be regarded as the total energy of
%    the system.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    15 November 2020
%
%  Author:
%
%    John Burkardt
%
%  Input:
%
%    real Y(:,2): the current solution.
%
%  Output:
%
%    real H(:): the value of the conserved quantity.
%
  u = y(:,1);
  v = y(:,2);

  [ g, l, m, t0, y0 ] = pendulum_parameters ( );

  h = 0.5 * m * g * l * u.^2 + 0.5 * m * v.^2;

  return
end

