function dxydt = arenstorf_deriv ( t, xy )

%*****************************************************************************80
%
%% arenstorf_deriv evaluates the right hand side of the Arenstorf ODE.
%
%  Discussion:
%
%    The Arenstorf ODE produces a stable periodic orbit for a three body 
%    problem involving the Earth, the Moon, and a spacecraft.
%
%    Although the orbit should be periodic, and repeats after a time interval
%    of a little more than 17 units, most ODE solvers will have difficulty
%    coming close to periodicity.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    08 February 2020
%
%  Author:
%
%    John Burkardt
%
%  Reference:
%
%    John D Cook,
%    The orbit that put men on the moon,
%    https://www.johndcook.com/blog/
%    Posted 08 February 2020.
%
%    Ernst Hairer, Syvert Norsett, Gerhard Wanner,
%    Solving Ordinary Differential Equations I: Nonstiff Problems,
%    Springer, 1987.
%
%  Input:
%
%    real T, XY(4): the time, and the position and speed of the spacecraft.
%
%  Output:
%
%    real DXYDT(4): the value of the derivative.
%
  x = xy(1);
  y = xy(2);
  xp = xy(3);
  yp = xy(4);
%
%  mu1 = relative mass of moon, mu2 = relative mass of earth.
%
  [ mu1, mu2, t0, xy0 ] = arenstorf_parameters ( );

  d1 = sqrt ( ( ( x + mu1 ).^2 + y.^2 ) .^3 );
  d2 = sqrt ( ( ( x - mu2 ).^2 + y.^2 ) .^3 );

  dxdt = xp;
  dydt = yp;
  dxpdt = x + 2.0 * yp - mu2 * ( x + mu1 ) / d1 - mu1 * ( x - mu2 ) / d2;
  dypdt = y - 2.0 * xp - mu2 * y / d1 - mu1 * y / d2;

  dxydt = [ dxdt; dydt; dxpdt; dypdt ];

  return
end
