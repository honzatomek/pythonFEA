function [ a, b, lam, mu, t0, y0 ] = rubber_band_parameters ( )

%*****************************************************************************80
%
%% rubber_band_parameters returns parameters for the rubber band ODE.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    12 November 2020
%
%  Author:
%
%    John Burkardt
%
%  Output:
%
%    real A, B: the coefficients of y+ and y-.
%
%    real LAM: the coefficient of the sinusoidal forcing term.
%
%    real MU: the coefficient of time in the sinusoidal forcing term.
%
%    real T0: the initial time.
%
%    real Y0: the initial condition at time T0.
%
  a = 17.0;
  b = 1.0;
  lam = 15.4;
  mu = 0.75;

  t0 = 0.0;
  y0 = [ 1.0, 0.0 ];

  return
end

