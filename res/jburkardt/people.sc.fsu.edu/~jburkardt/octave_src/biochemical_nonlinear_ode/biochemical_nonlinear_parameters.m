function [ a, b, kc, kn, rmax, e, t0, y0 ] = biochemical_nonlinear_parameters ( )

%*****************************************************************************80
%
%% biochemical_nonlinear_parameters: parameters for biochemical npnlinear ODE.
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
%    real KC, KN: parameters
%
%    real RMAX: a parameter.
%
%    real E: a parameter.
%
%    real T0: the initial time.
%
%    real Y0[4]: the initial condition at time T0.
%
  a = 1.0;
  b = 1.0;
  kc = 1.0;
  kn = 1.0;
  rmax = 1.0;
  e = 0.3;
  t0 = 0.0;
  y0 = [ 29.98; 9.98; 0.01; 0.01 ];

  return
end

