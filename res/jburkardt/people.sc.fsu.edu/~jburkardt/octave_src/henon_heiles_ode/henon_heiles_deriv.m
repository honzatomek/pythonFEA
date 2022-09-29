function dwdt = henon_heiles_deriv ( t, w )

%*****************************************************************************80
%
%% henon_heiles_deriv evaluates the derivative of the Henon Heiles ODE.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    13 November 2020
%
%  Author:
%
%    John Burkardt
%
%  Reference:
%
%    Michel Henon, Carl Heiles,
%    The applicability of the third integral of motion: 
%    Some numerical experiments,
%    The Astronomical Journal,
%    Volume 69, pages 73-79, 1964.
%
%  Input:
%
%    real T, W(4): the arguments of the derivative.
%
%  Output:
%
%    real DWDT(4): the value of the derivative.
%
  [ e, lam, t0, w0 ] = henon_heiles_parameters ( );

  x =  w(1);
  xp = w(2);
  y =  w(3);
  yp = w(4);

  dxdt = xp;
  dxpdt = - x - 2.0 * lam * x * y;
  dydt = yp;
  dypdt = - y - lam * ( x.^2 - y.^2 );

  dwdt = [ dxdt; dxpdt; dydt; dypdt ];

  return
end

