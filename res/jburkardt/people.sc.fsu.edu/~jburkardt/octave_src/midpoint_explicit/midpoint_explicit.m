function [ t, y ] = midpoint_explicit ( f, tspan, y0, n )

%*****************************************************************************80
%
%% midpoint_explicit uses the explicit midpoint method to solve an ODE.
%
%  Discussion:
%
%    The explicit midpoint method is sometimes called the modified Euler method.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    05 April 2021
%
%  Author:
%
%    John Burkardt
%
%  Input:
%
%    function handle f: evaluates the right hand side of the ODE.  
%
%    real tspan(2): the starting and ending times.
%
%    real y0(m): the initial conditions. 
%
%    integer n: the number of steps.
%
%  Output:
%
%    real t(n+1,1), y(n+1,m): the solution estimates.
%
  m = length ( y0 );
  t = zeros ( n + 1, 1 );
  y = zeros ( n + 1, m );

  dt = ( tspan(2) - tspan(1) ) / n;

  t(1) = tspan(1);
  y(1,:) = y0(:);

  for i = 1 : n

    tm = t(i)   + 0.5 * dt;
    ym = y(i,:) + 0.5 * dt * transpose ( f ( t(i), y(i,:) ) );

    t(i+1)   = t(i)   + dt;
    y(i+1,:) = y(i,:) + dt * transpose ( f ( tm, ym ) );

  end

  return
end

