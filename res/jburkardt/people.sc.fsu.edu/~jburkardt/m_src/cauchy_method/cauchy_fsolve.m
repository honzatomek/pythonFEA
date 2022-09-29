function [ t, y ] = cauchy_fsolve ( f, tspan, y0, n, theta )

%*****************************************************************************80
%
%% cauchy_fsolve uses the Cauchy method + fsolve() to solve an ODE.
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
  options = optimoptions ( 'fsolve', 'Display', 'off' );

  m = length ( y0 );
  t = zeros ( n + 1, 1 );
  y = zeros ( n + 1, m );

  dt = ( tspan(2) - tspan(1) ) / n;

  t(1,1) = tspan(1);
  y(1,:) = y0(:);

  for i = 1 : n

    to = t(i,1);
    yo = y(i,:);

    tc = to + theta * dt; 
    yc = yo + theta * dt * transpose ( f ( to, yo ) );
    yc = fsolve ( @(yc)backward_euler_residual(f,to,yo,tc,yc), yc, options );

    tp = to + dt;
    yp = ( yc + ( theta - 1.0 ) * yo ) / theta;

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
%    real to, yo: the old time and solution value.
%
%    real tp, yp: the new time and solution value.
%
%  Output:
%
%    real value: the backward Euler residual.
%
  value = yp - yo - ( tp - to ) * transpose ( f ( tp, yp ) );

  return
end


