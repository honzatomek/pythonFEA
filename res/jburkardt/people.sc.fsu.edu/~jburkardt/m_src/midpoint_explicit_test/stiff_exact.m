function value = stiff_exact ( t )

%*****************************************************************************80
%
%% stiff_exact evaluates the exact solution of the stiff equation.
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
%    real T(:): the evaluation times.
%
%  Output:
%
%    real Y(:): the exact solution values.
%
  value = 50.0 * ( sin ( t ) + 50.0 * cos(t) - 50.0 * exp ( - 50.0 * t ) ) / 2501.0;

  return
end

