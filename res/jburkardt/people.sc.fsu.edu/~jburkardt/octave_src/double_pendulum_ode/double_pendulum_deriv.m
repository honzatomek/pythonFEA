function dxdt = double_pendulum_deriv ( t, x )

%*****************************************************************************80
%
%% double_pendulum_deriv: double pendulum ordinary differential equation.
%
%  Discussion:
%
%    In the double pendulum problem, a rod of length l1 is fixed at
%    one end (0,0), and forms an angle theta1 with the downward vertical,
%    so that its endpoint is at (x1,y1) = (l1*cos(theta1),l1*sin(theta1)).
%    A weight of mass m1 is attached to this end of the first rod.
%
%    A second rod, of length l2 is also attached to this end of the
%    first rod.  It forms an angle theta2 with the downward vertical.
%    A weight of mass m2 is attached to the free end of the second rod.
%    The position of this weight is 
%    (x2,y2) = (x1,y1) + (l2*cos(theta2),l2*sin(theta2)).
%
%    Gravity has a force coefficient of G.
%
%    The unknowns are the angles and their time derivatives.
%    We store the unknowns in a vector X:
%
%      x(1) =   theta(1)
%      x(2) = d theta(1) dt
%      x(3) =   theta(2)
%      x(4) = d theta(2) dt
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    11 November 2020
%
%  Author:
%
%    Alexander Erlich (alexander.erlich@gmail.com)
%    Modifications by John Burkardt
%
%  Input:
%
%    real T: the current time.
%
%    real X(4): the current solution.
%
%  Output:
%
%    real DXDT(4): the right hand side of the ODE.
%
%  Local:
%
%    real G: the gravitational force coefficient.
%
%    real M1, M2: the masses of pendulums 1 and 2.
%
%    real L1, L2: the lengths of pendulums 1 and 2.
%
  [ g, m1, m2, l1, l2, t0, y0 ] = double_pendulum_parameters ( );

  dxdt = zeros(4,1);

  dxdt(1) = x(2);

  dxdt(2) = - ...
  ( ...
    (...
      g * ( 2.0 * m1 + m2 ) * sin ( x(1) ) + ...
      m2 * ...
      ( ...
        g * sin ( x(1) - 2.0 * x(3) ) + 2.0 * ...
        ( ...
          l2 * x(4)^2 + l1 * x(2)^2 * cos ( x(1) - x(3) ) ...
        ) ...
        * sin ( x(1) - x(3) ) ...
      ) ...
    ) ...
    / ...
    ( ...
     2.0 * l1 * ( m1 + m2 - m2 * cos ( x(1) - x(3) )^2 ) ...
    ) ...
  );

  dxdt(3) = x(4);

  dxdt(4) = ...
  ( ...
    ( ...
      ( m1 + m2 ) * ( l1 * x(2)^2 + g * cos ( x(1) ) ) ...
      + l2 * m2 * x(4)^2 * cos ( x(1) - x(3) ) ...
    ) * ...
    sin ( x(1) - x(3) ) ...
  ) ...
  / ...
  ( ...
    l2 * ( m1 + m2 - m2 * cos ( x(1) - x(3) )^2 ) ...
  );

  return
end

