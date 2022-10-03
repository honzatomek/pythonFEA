function dydt = stiff_deriv ( t, y )

%*****************************************************************************80
%
%% stiff_deriv evaluates the right hand side of the stiff equation.
%
%  Discussion:
%
%    y' = 50 * ( cos(t) - y )
%    y(0) = 0
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    06 February 2020
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
  dydt = 50.0 * ( cos ( t ) - y );

  return
end

