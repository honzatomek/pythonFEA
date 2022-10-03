function [ t, y ] = midpoint_fixed ( f, tspan, y0, n )

%*****************************************************************************80
%
%% midpoint_fixed uses a fixed-point midpoint method to solve an ODE.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    11 October 2020
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

  it_max = 10;
  theta = 0.5;

  t(1,1) = tspan(1);
  y(1,:) = y0(:);

  for i = 1 : n
    xm = t(i,1) + theta * dt; 
    ym(1,:) = y(i,:);
    for j = 1 : it_max
      ym(1,:) = y(i,:) + theta * dt * ( f ( xm, ym(1,:) ) )';
    end
    t(i+1,1) = t(i,1) + dt;
    y(i+1,:) = (       1.0 / theta ) * ym(1,:) ...
             + ( 1.0 - 1.0 / theta ) * y(i,:);
  end

  return
end

