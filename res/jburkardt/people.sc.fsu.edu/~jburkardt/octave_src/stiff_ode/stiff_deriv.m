function dydt = stiff_deriv ( t, y )

%*****************************************************************************80
%
%% stiff_deriv() evaluates the right hand side of the stiff ODE.
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
%    real T, Y: the time and solution value.
%
%  Output:
%
%    real DYDT: the derivative value.
%
  [ lambda, t0, y0 ] = stiff_parameters ( );

  dydt = lambda * ( cos ( t ) - y );

  return
end

