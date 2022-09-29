function e = double_pendulum_conserved ( x )

%*****************************************************************************80
%
%% double_pendulum_conserved evaluates a conserved quantity for the double pendulum.
%
%  Discussion:
%
%    This quantity is the total energy.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    13 November 2020
%
%  Author:
%
%    John Burkardt
%
%  Input:
%
%    real X(4): theta1, dtheta1/dt, theta2, dtheta2/dt.
%
%  Output:
%
%    real E: the energy.
%
  [ g, m1, m2, l1, l2, t0, y0 ] = double_pendulum_parameters ( );

  e = 0.5 * ( m1 + m2 ) * l1^2 * x(:,2).^2 ...
    + 0.5 * m2 * l2^2 * x(:,4).^2 ...
    + m2 * l1 * l2 * x(:,2) .* x(:,4) .* cos ( x(:,1) - x(:,3) ) ...
    - ( m1 + m2 ) * g * l1 * cos ( x(:,1) ) ...
    - m2 * g * l2 * cos ( x(:,3) );

  return
end

