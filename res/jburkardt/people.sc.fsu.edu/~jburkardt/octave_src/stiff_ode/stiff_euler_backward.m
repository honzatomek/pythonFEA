function [ t, y ] = stiff_euler_backward ( n )

%*****************************************************************************80
%
%% stiff_euler_backward() uses the backward Euler method on the stiff ODE.
%
%  Discussion:
%
%    y' = lambda * ( cos(t) - y )
%    y(t0) = y0
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    06 April 2021
%
%  Author:
%
%    John Burkardt
%
%  Input:
%
%    integer N: the number of steps to take.
%
%  Output:
%
%    real T(N+1), Y(N+1): the times and estimated solutions.
%
  t = zeros ( n + 1, 1 );
  y = zeros ( n + 1, 1 );

  [ lambda, t0, y0 ] = stiff_parameters ( );

  tstop = 1.0;
  dt = ( tstop - t0 ) / n;

  t(1) = t0;
  y(1) = y0;

  for i = 1 : n
    t(i+1) = t(i) + dt;
    y(i+1) = ( y(i) + dt * lambda * cos ( t(i+1) ) ) / ( 1.0 + lambda * dt );
  end

  return
end

