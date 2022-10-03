function yprime = quasiperiodic_deriv ( t, y )

%*****************************************************************************80
%
%% quasiperiodic_deriv returns the right hand side of the quasiperiodic ODE.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    23 July 2020
%
%  Author:
%
%    John Burkardt
%
%  Reference:
%
%  Input:
%
%    real T, the current time.
%
%    real Y(4,1), the current state values.
%
%  Output:
%
%    real YPRIME(4,1), the time derivatives of the current state values.
%
  yprime = zeros ( size ( y ) );

  yprime(1,:) = y(2,:);
  yprime(2,:) = y(3,:);
  yprime(3,:) = y(4,:);
  yprime(4,:) = - ( pi^2 + 1.0 ) * y(3,:) - pi^2 * y(1,:);

  return
end

