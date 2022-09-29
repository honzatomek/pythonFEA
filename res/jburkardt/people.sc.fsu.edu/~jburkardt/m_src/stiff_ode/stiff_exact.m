function value = stiff_exact ( t )

%*****************************************************************************80
%
%% stiff_exact() evaluates the exact solution of stiff_ode().
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
%    real T(:): the evaluation times.
%
%  Output:
%
%    real Y(:): the exact solution values.
%
  [ lambda, t0, y0, tstop ] = stiff_parameters ( );

  value = lambda * ...
    ( sin ( t ) + lambda * cos(t) - lambda * exp ( - lambda * t ) ) ...
    / ( lambda^2 + 1.0 );

  return
end

