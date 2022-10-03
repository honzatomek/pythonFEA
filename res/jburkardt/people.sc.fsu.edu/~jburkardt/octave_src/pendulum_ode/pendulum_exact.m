function y = pendulum_exact ( t )

%*****************************************************************************80
%
%% pendulum_exact returns the exact solution for the pendulum ODE.
%
%  Discussion:
%
%    This solution satisfies the pendulum ODE:
%
%    u' = v
%    v' = - sqrt ( g / l ) * u
%
%    u(t0) = u0 = y0(1)
%    v(t0) = v0 = y0(2)
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
%    real T(:): the current time.
%
%  Output:
%
%    real Y(2): the exact solution.
%
  [ g, l, m, t0, y0 ] = pendulum_parameters ( );

  w = sqrt ( g / l );

  u =               y0(1) * cos ( w * ( t - t0 ) ) ...
    + ( 1.0 / w ) * y0(2) * sin ( w * ( t - t0 ) );

  v =       - w   * y0(1) * sin ( w * ( t - t0 ) ) ...
    +               y0(2) * cos ( w * ( t - t0 ) );

  y = [ u, v ];

  return
end

