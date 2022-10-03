function [ t, y, e ] = rk12 ( yprime, tspan, y0, n )

%*****************************************************************************80
%
%% rk12 uses explicit Runge-Kutta schemes of order 1 and 2.
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
%    07 March 2021
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
%    real e(n+1,m): an estimate for the error made at each step.
% 
  m = length ( y0 );
  t = zeros ( n + 1, 1 );
  y = zeros ( n + 1, m );
  e = zeros ( n + 1, m );

  dt = ( tspan(2) - tspan(1) ) / n;

  t(1,1) = tspan(1);
  y(1,:) = y0(:);
  e(1,:) = 0.0;
 
  for i = 1 : n
    k1 = dt * yprime ( t(i),      y(i,:) );
    yt = y(i) + transpose ( k1 );
    k2 = dt * yprime ( t(i) + dt, y(i,:) + transpose ( k1 ) );
    y(i+1,:) = y(i,:) + 0.5 * transpose ( k1 ) + 0.5 * transpose ( k2 );
    e(i+1,:) = y(i+1,:) - yt;
  end

  return
end

