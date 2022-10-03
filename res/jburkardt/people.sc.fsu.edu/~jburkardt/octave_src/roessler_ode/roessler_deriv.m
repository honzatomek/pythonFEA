function dydt = roessler_deriv ( t, y )

%*****************************************************************************80
%
%% roessler_deriv evaluates the right hand side of the Roessler ODE.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%   11 November 2020
%
%  Author:
%
%    John Burkardt
%
%  Reference:
%
%    Otto Roessler,
%    An Equation for Continuous Chaos,
%    Physics Letters, 
%    Volume 57A, Number 5, pages 397–398, 1976.
%
%  Input:
%
%    real T: the value of the independent variable.
%
%    real Y(3): the values of the dependent variables at time T.
%
%  Output:
%
%    real DYDT(3): the right hand side of the ODE.
%
  [ alpha, beta, mu, t0, y0 ] = roessler_parameters ( );

  dy1dt = - y(2) - y(3);
  dy2dt = y(1) + alpha * y(2);
  dy3dt = beta + ( y(1) - mu ) * y(3);

  dydt = [ dy1dt; dy2dt; dy3dt ];

  return
end
