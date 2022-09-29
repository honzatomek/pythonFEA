function dydt = rubber_band_deriv ( t, y )

%*****************************************************************************80
%
%% rubber_band_deriv evaluates the right hand side of the rubber band ODE.
%
%  Discussion:
%
%    The original equation has the form:
%
%      y'' + 0.01 y' + a y(+) - b y(-) = 10 + lam sin ( mu t )
%
%    where y(+)=max(y,0) and y(-)=max(-y,0).
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license. 
%
%  Modified:
%
%    26 April 2020
%
%  Author:
%
%    Original Python version by John D Cook.
%    MATLAB version by John Burkardt
%
%  Reference:
%
%    Lisa Humphreys, Ray Shammas,
%    Finding Unpredictable Behavior in a Simple Ordinary Differential Equation. 
%    The College Mathematics Journal, 
%    Volume 31, Number 5, November 2000, pages 338-346.
%
%    John D Cook,
%    A spring, a rubber band, and chaos
%    https://www.johndcook.com/blog/
%    26 April 2020.
%
%  Input
%
%    real T: the current time.
%
%    real Y(2): the current values of Y and Y'.
%
%  Output:
%
%    real DYDT(2): the current values of Y' and Y''.
%
  [ a, b, lam, mu, t0, y0 ] = rubber_band_parameters ( );

  u = y(1);
  v = y(2);

  up = y(2);
  vp = 10.0 + lam * sin ( mu * t ) - 0.01 * v - a * max ( u, 0.0 ) ...
    + b * max ( -u, 0.0 );
 
  dydt = [ up; vp ];

  return
end
