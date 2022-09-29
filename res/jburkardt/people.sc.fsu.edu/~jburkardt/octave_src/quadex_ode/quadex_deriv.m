function dydt = quadex_deriv ( t, y )

%*****************************************************************************80
%
%% quadex_deriv evaluates the right hand side of the "quadex" ODE.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    19 October 2020
%
%  Author:
%
%    John Burkardt
%
%  Input:
%
%    real T(:), Y(:), the arguments of the derivative.
%
%  Output:
%
%    real DYDT(:), the value of the derivative.
%
  dydt = 5.0 * ( y - t.^2 );

  return
end

