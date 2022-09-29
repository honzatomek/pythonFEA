function ydot = three_body_deriv ( t, y )

%*****************************************************************************80
%
%% three_body_deriv returns the right hand side of the three body ODE system.
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
%    Original MATLAB version by Dominik Gruntz, Joerg Waldvogel.
%
%  Reference:
%
%    Dominik Gruntz, Joerg Waldvogel,
%    Orbits in the Planar Three-Body Problem,
%    Walter Gander, Jiri Hrebicek, editors,
%    Solving Problems in Scientific Computing using Maple and Matlab,
%    Springer, 1997,
%    ISBN: 3-540-61793-0,
%    LC: Q183.9.G36.
%
%  Input:
%
%    real T, the current time.
%
%    real Y(12), the current solution.
%
%  Output:
%
%    real YDOT(12), the derivatives of the current solution.
%
  [ m0, m1, m2, t0, y0 ] = three_body_parameters ( );

  x0 = y(1:2);
  x1 = y(5:6);
  x2 = y(9:10);

  d0 = ( x2 - x1 ) / norm ( x2 - x1 )^3;
  d1 = ( x0 - x2 ) / norm ( x0 - x2 )^3;
  d2 = ( x1 - x0 ) / norm ( x1 - x0 )^3;

  ydot(1:2) = y(3:4);
  ydot(3:4) = m1 * d2 - m2 * d1;
  ydot(5:6) = y(7:8);
  ydot(7:8) = m2 * d0 - m0 * d2;
  ydot(9:10) = y(11:12);
  ydot(11:12) = m0 * d1 - m1 * d0;

  ydot = ydot(:);

  return
end
