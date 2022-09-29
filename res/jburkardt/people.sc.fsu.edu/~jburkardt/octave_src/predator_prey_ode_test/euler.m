function [ t, y ] = euler ( dydt, tspan, y0, n )

%*****************************************************************************80
%
%% euler approximates the solution to an ODE using Euler's method.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    21 March 2020
%
%  Author:
%
%    John Burkardt
%
%  Input:
%
%    function handle dydt: points to a function that evaluates the right
%    hand side of the ODE.
%
%    real tspan(2): contains the initial and final times.
%
%    real y0(m): a column vector containing the initial condition.
%
%    integer n: the number of steps to take.
%
%  Output:
%
%    real t(n+1,1), y(n+1,m): the times and solution values.
%
  m = length ( y0 );
  t = zeros ( n + 1, 1 );
  y = zeros ( n + 1, m );

  dt = ( tspan(2) - tspan(1) ) / n;

  t(1,1) = tspan(1);
  y(1,:) = y0(:);

  for i = 1 : n
    t(i+1,1) = t(i,1) + dt;
    y(i+1,:) = y(i,:) + dt * ( dydt ( t(i,1), y(i,:) ) )';
  end

  return
end

