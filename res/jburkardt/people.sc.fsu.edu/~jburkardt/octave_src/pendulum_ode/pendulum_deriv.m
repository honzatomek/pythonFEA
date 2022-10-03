function dydt = pendulum_deriv ( t, y )

%*****************************************************************************80
%
%% pendulum_deriv returns the right hand side of the linear pendulum ODE.
%
%  Discussion:
%
%    Y1 is the angular displacement;
%    Y2 is the angular velocity;
%
%    G is the gravitational coefficient.
%    L is the length of the pendulum.
%    M is the pendulum mass.
%
%    u' = v
%    v' = - ( g / l ) * u
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    31 October 2020
%
%  Author:
%
%    John Burkardt
%
%  Input:
%
%    real T, the current time.
%
%    real Y(2), the current state values.
%
%  Output:
%
%    real dydt(2), the time derivatives of the current state values.
%
  [ g, l, m, t0, y0 ] = pendulum_parameters ( );

  u = y(1);
  v = y(2);

  dudt = v;
  dvdt = - ( g / l ) * u;

  dydt = [ dudt; dvdt ];

  return
end

