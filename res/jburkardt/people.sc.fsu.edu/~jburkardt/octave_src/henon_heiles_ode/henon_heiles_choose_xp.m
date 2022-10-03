function w0 = henon_heiles_choose_xp ( e, lam, t0, x, y, yp )

%*****************************************************************************80
%
%% henon_heiles_choose_xp: consistent initial condition for Henon Heiles ODE.
%
%  Discussion:
%
%    It must be the case that 0 <= 2e-yp^2-x^2-y^2-2x^2y+2y^3/3.
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
%    real E: proposed initial energy.
%
%    real LAM: a parameter.
%
%    real T0: the initial time.
%
%    real X, Y, YP: proposed initial values for three coordinates.
%
%  Output:
%
%    real W0[4]: a consistent initial condition.
%
  xpsq = 2.0 * e - yp.^2 - x.^2 - y.^2  ...
    - 2.0 * lam * ( x.^2 .* y - y.^3 / 3.0 );

  if ( xpsq < 0.0 )
    fprintf ( 1, 'henon_heiles_choose_xp - Fatal error!\n' );
    fprintf ( 1, '  2e-yp^2-x^2-y^2-2x^2y+2y^3/3 < 0\n' );
    error ( 'henon_heiles_choose_xp - Fatal error!' );
  end

  xp = sqrt ( xpsq );

  w0 = [ x, xp, y, yp ];

  return
end

