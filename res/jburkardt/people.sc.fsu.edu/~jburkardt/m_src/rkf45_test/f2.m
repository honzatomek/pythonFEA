function yp = f2 ( t, y )

%*****************************************************************************80
%
%% f2() evaluates the derivative for the ODE.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    30 August 2010
%
%  Author:
%
%    John Burkardt
%
%  Input:
%
%    real T, the value of the independent variable.
%
%    real Y(NEQN), the value of the dependent variable.
%
%  Output:
%
%    real YP(NEQN), the value of the derivative dY(1:NEQN)/dT.
%
  yp = [ y(2); -y(1) ];

  return
end
