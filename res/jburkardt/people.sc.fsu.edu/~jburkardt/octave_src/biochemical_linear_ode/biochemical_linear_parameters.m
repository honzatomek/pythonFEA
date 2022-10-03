function [ a, b, t0, y0 ] = biochemical_linear_parameters ( )

%*****************************************************************************80
%
%% biochemical_linear_parameters returns parameters for the biochemical linear ODE.
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
%    real A, B: parameters;
%
%    real T0: the initial time.
%
%    real Y0[2]: the initial condition at time T0.
%
  a = 1.0;
  b = 1.0;
  t0 = 0.0;
  y0 = [ 29.98; 9.98 ];

  return
end

