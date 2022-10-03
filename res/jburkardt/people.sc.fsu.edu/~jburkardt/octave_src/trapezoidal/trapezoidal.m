function [ t, y ] = trapezoidal ( f, tspan, y0, n )

%*****************************************************************************80
%
%% trapezoidal() uses the trapezoidal method + fsolve() to solve an ODE.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    26 April 2021
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

    tn = to + dt; 
    yn = yo + dt * ( f ( to, yo ) )';

    yn = fsolve ( @(yn)trapezoidal_residual(f,to,yo,tn,yn), yn );

    t(i+1,1) = tn;
    y(i+1,:) = yn;

  end

  return
end
function value = trapezoidal_residual ( f, to, yo, tn, yn )

%*****************************************************************************80
%
%% trapezoidal()_residual evaluates the trapezoidal residual.
%
%  Discussion:
%
%    We are seeking a value YN defined by the implicit equation:
%
%      YN = YO + 0.5 * ( TN - TO ) * ( F ( TO, YO ) + F ( TN, YN ) )
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    15 October 2020
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
%    real tn, yn: the new time and solution value.
%
%  Output:
%
%    real value: the trapezoidal residual.
%
  value = yn - yo - 0.5 * ( tn - to ) * ( f ( to, yo ) + f ( tn, yn ) )';

  return
end

