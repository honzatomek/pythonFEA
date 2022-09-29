function h = pendulum_nonlinear_conserved ( y )

%*****************************************************************************80
%
%% pendulum_nonlinear_conserved returns a conserved quantity for the nonlinear pendulum ODE.
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
%  Input:
%
%    real Y(2,:): the current solution.
%
%  Output:
%
%    real H(:): the value of the conserved quantity.
%
  g = 9.8;
  l = 1.0;

  u = y(1,:);
  v = y(2,:)

  h = ( g / l ) * u.^2 + v.^2;

  return
end

