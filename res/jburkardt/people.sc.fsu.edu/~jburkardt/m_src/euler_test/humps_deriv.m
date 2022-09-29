function dydt = humps_deriv ( t, y )

%*****************************************************************************80
%
%% humps_deriv returns the derivative for the humps ODE.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    25 April 2020
%
%  Author:
%
%    John Burkardt
%
%  Input:
%
%    real T: the current time.
%
%    real Y: the current solution value.
%
%  Output:
%
%    real DYDT: the value of dY/dT.
%    
  dydt = - 2.0 * ( t - 0.3 ) / ( ( t - 0.3 )^2 + 0.01 )^2 ...
         - 2.0 * ( t - 0.9 ) / ( ( t - 0.9 )^2 + 0.04 )^2;

  return
end

