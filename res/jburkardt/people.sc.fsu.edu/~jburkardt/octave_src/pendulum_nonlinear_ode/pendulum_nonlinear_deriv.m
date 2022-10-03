function yprime = pendulum_nonlinear_deriv ( t, y )

%*****************************************************************************80
%
%% pendulum_nonlinear_deriv returns the right hand side of the nonlinear pendulum ODE.
%
%  Discussion:
%
%    Y1 is the angular displacement;
%    Y2 is the angular velocity;
%    L is the length of the pendulum (set to 1 here);
%    G is the gravitational coefficient (set to 9.8 here).
%
%    y1' = y2
%    y2' = - ( g / l ) * sin ( y1 )
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    23 September 2018
%
%  Author:
%
%    John Burkardt
%
%  Input:
%
%    real T, the current time.
%
%    real Y(2,1), the current state values.
%
%  Output:
%
%    real YPRIME(2,1), the time derivatives of the current state values.
%
  g = 9.8;
  l = 1.0;

  yprime(1,1) = y(2);
  yprime(2,1) = - ( g / l ) * sin ( y(1) );

  return
end

