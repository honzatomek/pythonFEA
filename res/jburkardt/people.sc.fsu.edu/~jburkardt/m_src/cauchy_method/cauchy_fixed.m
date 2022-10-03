function [ t, y ] = cauchy_fixed ( f, tspan, y0, n, theta )

%*****************************************************************************80
%
%% cauchy_fixed uses the Cauchy method + fixed point to solve an ODE.
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
%    function handle f: evaluates the right hand side of the ODE.  
%
%    real tspan(2): the starting and ending times.
%
%    real y0(m): the initial conditions. 
%
%    integer n: the number of steps.
%
%    real theta: the value of theta, 0 < theta <= 1.
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

  t(1,1) = tspan(1);
  y(1,:) = y0(:);

  for i = 1 : n
    xm = t(i,1) + theta * dt; 
    ym(1,:) = y(i,:);
    for j = 1 : it_max
      ym(1,:) = y(i,:) + theta * dt * transpose ( f ( xm, ym(1,:) ) );
    end
    t(i+1,1) = t(i,1) + dt;
    y(i+1,:) = (       1.0 / theta ) * ym(1,:) ...
             + ( 1.0 - 1.0 / theta ) * y(i,:);
  end

  return
end

