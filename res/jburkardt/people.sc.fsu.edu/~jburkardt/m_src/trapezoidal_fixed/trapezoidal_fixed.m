function [ t, y ] = trapezoidal_fixed ( f, tspan, y0, n )

%*****************************************************************************80
%
%% trapezoidal_fixed() uses a fixed-point trapezoidal method to solve an ODE.
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

  it_max = 10;

  t(1,1) = tspan(1);
  y(1,:) = y0(:);

  for i = 1 : n

    to = t(i,1);
    yo(1,:) = y(i,:);

    tn = to + dt;
    yn(1,:) = yo(1,:) + dt * transpose ( f ( to, yo(1,:) ) );

    for j = 1 : it_max
      yn(1,:) = yo(1,:) ...
        + 0.5 * dt * transpose ( f ( to, yo(1,:) ) + f ( tn, yn(1,:) ) );
    end

    t(i+1,1) = tn;
    y(i+1,:) = yn(1,:);

  end

  return
end

