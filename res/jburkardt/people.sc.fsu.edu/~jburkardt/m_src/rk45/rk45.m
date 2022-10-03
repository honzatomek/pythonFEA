function [ t, y, e ] = rk45 ( yprime, tspan, y0, n )

%*****************************************************************************80
%
%% rk45 uses explicit Runge-Kutta schemes of order 4 and 5.
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
%    real e(n+1,m): an estimate for the additional error
%    made at each step.
% 
  m = length ( y0 );
  t = zeros ( n + 1, 1 );
  y = zeros ( n + 1, m );
  e = zeros ( n + 1, m );

  dt = ( tspan(2) - tspan(1) ) / n;

  t(1,1) = tspan(1);
  y(1,:) = y0(:);
  e(1,:) = 0.0;
 
  a = [           0.0,            0.0,            0.0,           0.0, 0.0;
                 0.25,            0.0,            0.0,           0.0, 0.0;
             3.0/32.0,       9.0/32.0,            0.0,           0.0, 0.0;
        1932.0/2197.0, -7200.0/2197.0,  7296.0/2197.0,           0.0, 0.0;
          439.0/216.0,           -8.0,   3680.0/513.0, -845.0/4104.0, 0.0;
            -8.0/27.0,            2.0, -3544.0/2565.0, 1859.0/4104.0, -11.0/40.0 ];
  b = [ 16.0/135.0, 0.0, 6656.0/12825.0, 28561.0/56430.0, -9.0/50.0, 2.0/55.0 ];
  c = [ 0.0, 0.25, 3.0/8.0, 12.0/13.0, 1.0, 0.5 ];
  d = [ 25.0/216.0, 0.0, 1408.0/2565.0, 2197.0/4104.0, -1.0/5.0, 0.0 ];

  for i = 1 : n

    k1 = dt * transpose ( yprime ( t(i) + c(1) * dt, y(i,:) ) );
    k2 = dt * transpose ( yprime ( t(i) + c(2) * dt, y(i,:) + a(2,1) * k1 ) );
    k3 = dt * transpose ( yprime ( t(i) + c(3) * dt, y(i,:) + a(3,1) * k1 ...
      + a(3,2) * k2 ) );
    k4 = dt * transpose ( yprime ( t(i) + c(4) * dt, y(i,:) + a(4,1) * k1 ...
      + a(4,2) * k2 + a(4,3) * k3 ) );
    k5 = dt * transpose ( yprime ( t(i) + c(5) * dt, y(i,:) + a(5,1) * k1 ...
      + a(5,2) * k2 + a(5,3) * k3 + a(5,4) * k4 ) );
    k6 = dt * transpose ( yprime ( t(i) + c(6) * dt, y(i,:) + a(6,1) * k1 ...
      + a(6,2) * k2 + a(6,3) * k3 + a(6,4) * k4 + a(6,5) * k5 ) );

    y4 = y(i,:) + d(1) * k1 + d(2) * k2 + d(3) * k3 ...
                + d(4) * k4 + d(5) * k5 + d(6) * k6;
    y5 = y(i,:) + b(1) * k1 + b(2) * k2 + b(3) * k3 ...
                + b(4) * k4 + b(5) * k5 + b(6) * k6;

    t(i+1,1) = t(i,1) + dt;
    y(i+1,:) = y5;
    e(i+1,:) = y5 - y4;

  end

  return
end

