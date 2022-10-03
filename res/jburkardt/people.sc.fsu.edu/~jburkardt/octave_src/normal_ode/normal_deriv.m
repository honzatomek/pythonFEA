function dydt = normal_deriv ( t, y )

%*****************************************************************************80
%
%% normal_deriv returns the right hand side of the NORMAL ODE.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    08 October 2020
%
%  Author:
%
%    John Burkardt
%
%  Input:
%
%    real T, Y, the arguments.
%
%  Output:
%
%    real DYDT, the value of the right hand side of the ODE.
%
  dydt = - t .* y;

  return
end

