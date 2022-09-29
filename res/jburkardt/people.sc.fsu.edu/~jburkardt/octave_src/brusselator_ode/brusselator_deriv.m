function dxydt = brusselator_deriv ( t, xy )

%*****************************************************************************80
%
%% brusselator_deriv defines the Brusselator ODE system.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    09 February 2020
%
%  Author:
%
%    John Burkardt
%
%  Reference:
%
%    John D Cook,
%    Behold! The Brusselator!,
%    https://www.johndcook.com/blog/
%    Posted 07 February 2020.
%
%    Rene Lefever, Gregoire Nicholis,
%    Chemical instabilities and sustained oscillations,
%    Journal of Theoretical Biology,
%    Volume 30, Issue 2, February 1971, Pages 267-284.
%
%  Input:
%
%    real T, the current time.
%
%    real XY(2), the current solution variables.
%
%  Output:
%
%    real DXYDT(2), the right hand side of the ODE's.
%
  [ a, b, t0, xy0 ] = brusselator_parameters ( );

  x = xy(1);
  y = xy(2);

  dxdt = a + x.^2 * y - ( b + 1.0 ) * x;
  dydt = b * x - x.^2 * y;

  dxydt = [ dxdt; dydt ];

  return
end
