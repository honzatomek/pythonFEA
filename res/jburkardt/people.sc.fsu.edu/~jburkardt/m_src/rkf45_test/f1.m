function yp = f1 ( t, y )

%*****************************************************************************80
%
%% f1() evaluates the derivative for the ODE.
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
%    real Y, the value of the dependent variable.
%
%  Output:
%
%    real YP, the value of the derivative dY(1:NEQN)/dT.
%
  yp = zeros ( size ( y ) );

  yp = 0.25 * y * ( 1.0 - y / 20.0 );

  return
end
 
