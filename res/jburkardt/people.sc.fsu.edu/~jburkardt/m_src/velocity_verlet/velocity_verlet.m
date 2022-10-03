function [ t, y ] = velocity_verlet ( dydt, tspan, y0, n )

%*****************************************************************************80
%
%% velocity_verlet solves an ODE using the Velocity Verlet method.
%
%  Discussion:
%
%    A fixed stepsize is used.
%
%    There should be just two variables:
%    U is the position;
%    V is the velocity.
%
%    The ODE should have the form:
%
%    U' = V
%    V' = F(T,U)
%
%    Or, as a second order system:
%
%    u'' = f(t,u)
%
%    where f does NOT depend on u'.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    29 April 2021
%
%  Author:
%
%    John Burkardt
%
%  Input:
%
%    function handle dydt: evaluates the right hand side of the ODE.
%
%    real tspan(2): the initial and final times.
%
%    real y0(2): the initial position and velocity.
%
%    integer n: the number of steps to take.
%
%  Output:
%
%    real t(n+1), y(n+1,2): the times, positions, and velocities.
%
  t0 = tspan(1);
  tstop = tspan(2);

  dt = ( tstop - t0 ) / n;

  t = ( linspace ( t0, tstop, n + 1 ) )';
  p = zeros ( n + 1, 1 );
  v = zeros ( n + 1, 1 );

  for i = 1 : n + 1
    if ( i == 1 )
      p(i) = y0(1);
      uvdot = dydt ( t(i), [ p(i), v(i) ] );
      dv = 0.5 * dt * uvdot(2);
      v(i) = y0(2);
    else
      w = v(i-1) + dv;
      p(i) = p(i-1) + dt * w;
      uvdot = dydt ( t(i), [ p(i), v(i) ] );
      dv = 0.5 * dt * uvdot(2);
      v(i) = w + dv;
    end
  end

  y = [ p, v ];

  return
end

