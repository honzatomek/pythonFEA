function [ t0, y0 ] = flame_parameters ( )

%*****************************************************************************80
%
%% flame_parameters returns parameters for the flame ODE.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    10 November 2020
%
%  Author:
%
%    John Burkardt
%
%  Output:
%
%    real T0: the initial time.
%
%    real Y0: the initial condition at time T0.
%
  t0 = 0.0;
  y0 = 0.01;

  return
end

