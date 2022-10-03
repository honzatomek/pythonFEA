function [ t, y ] = rk3 ( yprime, tspan, y0, n )

%*****************************************************************************80
%
%% rk3 uses an explicit Runge-Kutta scheme of order 3.
%
%  Discussion:
%
%    This function approximates the solution of a differential
%    equation of the form:
%      y' = yprime(t,y)
%      y(tspan(1)) = y0
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    22 March 2020
%
%  Author:
%
%    John Burkardt
%
%  Input:
%
%    function handle yprime: defines the derivative function of the form
%      value = yprime ( t, y )
%
%    real tspan(2): contains the initial and final times.
%
%    real y0(m): contains the initial condition.
%
%    integer n: the number of steps to take.
%
%  Output:
%
%    real t(n+1): the times.
%
%    real y(n+1,m): the estimated solution values.
%
  m = length ( y0 );
  t = zeros ( n + 1, 1 );
  y = zeros ( n + 1, m );

  dt = ( tspan(2) - tspan(1) ) / n;

  t(1,1) = tspan(1);
  y(1,:) = y0(:);

  for i = 1 : n

    k1 = dt * yprime ( t(i),            y(i,:) );
    k2 = dt * yprime ( t(i) + dt,       y(i,:) + k1' );
    k3 = dt * yprime ( t(i) + 0.5 * dt, y(i,:) + 0.25 * k1' + 0.25 * k2' );

    y3 = y(i,:) + ( k1' + k2' + 4.0 * k3' ) / 6.0;

    t(i+1,1) = t(i) + dt;
    y(i+1,:) = y3;

  end

  return
end

