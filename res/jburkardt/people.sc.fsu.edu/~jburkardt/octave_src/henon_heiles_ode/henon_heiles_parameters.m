function [ e, lam, t0, w0 ] = henon_heiles_parameters ( )

%*****************************************************************************80
%
%% henon_heiles_parameters returns parameters for the henon heiles ODE.
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
%  Output:
%
%    real E: the initial energy.
%
%    real LAM: a parameter.
%
%    real T0: the initial time.
%
%    real W0(4): the initial condition at time T0.
%
  e = 0.05;
  lam = 1.0;

  t0 = 0.0;

  x = 0.0;
  y = 0.0;
  yp = 0.0;
  w0 = henon_heiles_choose_xp ( e, lam, t0, x, y, yp );

  return
end

