function [ t, y ] = midpoint ( f, tspan, y0, n )

%*****************************************************************************80
%
%% midpoint() uses the midpoint method + fsolve() to solve an ODE.
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

  t(1,1) = tspan(1);
  y(1,:) = y0(:);

  for i = 1 : n

    to = t(i,1);
    yo = y(i,:);

    th = to + 0.5 * dt; 
    yh = yo + 0.5 * dt * f ( to, yo )';
    yh = fsolve ( @(yh)backward_euler_residual(f,to,yo,th,yh), yh );

    tp = to + dt;
    yp = 2.0 * yh - yo;

    t(i+1,1) = tp;
    y(i+1,:) = yp;

  end

  return
end
function value = backward_euler_residual ( f, to, yo, tp, yp )

%*****************************************************************************80
%
%% backward_euler_residual evaluates the backward Euler residual.
%
%  Discussion:
%
%    We are seeking a value YP defined by the implicit equation:
%
%      YP = YO + ( TP - TO ) * F ( TP, YP )
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
%    real to, yo: the old time and solution value.
%
%    real tp, yp: the new time and solution value.
%
%  Output:
%
%    real value: the backward Euler residual.
%
  value = yp - yo - ( tp - to ) * ( f ( tp, yp ) )';

  return
end

