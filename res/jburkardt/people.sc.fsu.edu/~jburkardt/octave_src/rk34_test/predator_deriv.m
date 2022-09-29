function value = predator_deriv ( t, y )

%*****************************************************************************80
%
%% predator_deriv returns the right hand side of the predator test problem.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    31 October 2019
%
%  Author:
%
%    John Burkardt
%
  r = y(1);
  f = y(2);

  drdt =   2.0 * r - 0.001 * r * f;
  dfdt = -10.0 * f + 0.002 * r * f;

  value = [ drdt; dfdt ];

  return
end

