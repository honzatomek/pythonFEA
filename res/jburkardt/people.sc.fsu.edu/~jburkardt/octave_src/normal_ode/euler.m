function [ t, y ] = euler ( dydt, tspan, y0, n )

%*****************************************************************************80
%
%% euler solves an ordinary differential equation using the Euler method.
%
%  Discussion:
%
%    This function approximates the solution of a differential
%    equation of the form:
%      y' = dydt(t,y)
%      y(t0) = y0
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    06 November 2019
%
%  Author:
%
%    John Burkardt
%
%  Input:
%
%    function handle dydt: defines the derivative function of the form
%      value = dydt ( t, y )
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
%    real y(n+,m): the estimated solution values.
%
  dt = ( tspan(2) - tspan(1) ) / n;

  t(1,1) = tspan(1);
  y(1,:) = y0(:);
 
  for i = 1 : n
    f1 = dydt ( t(i,1), y(i,:) );
    t(i+1,1) = t(i,1) + dt;
    y(i+1,:) = y(i,:) + dt * f1';
  end

  return
end

