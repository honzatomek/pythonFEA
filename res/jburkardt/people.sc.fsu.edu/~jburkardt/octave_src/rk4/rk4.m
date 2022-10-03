function [ t, y ] = rk4 ( dydt, tspan, y0, n )

%*****************************************************************************80
%
%% rk4 approximates an ODE using a Runge-Kutta fourth order method.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    22 April 2020
%
%  Author:
%
%    John Burkardt
%
%  Input:
%
%    function handle dydt: a function that evaluates the right hand side.
%
%    real tspan(2): contains the initial and final times.
%
%    real y0(m): a column vector containing the initial condition.
%
%    integer n: the number of steps to take.
%
%  Output:
%
%    real t(n+1), y(n+1,m): the times and solution values.
%

%
%  Force y0 to be a column vector.
%
  y0 = y0(:);

  m = size ( y0, 1 );
  t = zeros ( n + 1, 1 );
  y = zeros ( n + 1, m );

  tfirst = tspan(1);
  tlast = tspan(2);
  dt = ( tlast - tfirst ) / n;

  t(1,1) = tspan(1);
  y(1,:) = y0(:);

  for i = 1 : n

    f1 = dydt ( t(i,1),            y(i,:) );
    f2 = dydt ( t(i,1) + dt / 2.0, y(i,:) + dt * f1' / 2.0 );
    f3 = dydt ( t(i,1) + dt / 2.0, y(i,:) + dt * f2' / 2.0 );
    f4 = dydt ( t(i,1) + dt,       y(i,:) + dt * f3' );

    t(i+1,1) = t(i,1) + dt;
    y(i+1,:) = y(i,:) + dt * ( f1' + 2.0 * f2' + 2.0 * f3' + f4' ) / 6.0;

  end

  return
end

