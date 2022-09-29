function [ mu1, mu2, t0, xy0 ] = arenstorf_parameters ( )

%*****************************************************************************80
%
%% arenstorf_parameters returns parameters for the arenstorf ODE.
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
%    real MU1, MU2: the relative masses of the moon and the earth.
%
%    real T0: the initial time.
%
%    real XY0(4): the initial condition at time T0.
%
  mu1 = 0.012277471;
  mu2 = 1.0 - mu1;

  t0 = 0.0;
  xy0 = [ 0.994, 0.0, 0.0, -2.00158510637908252240537862224 ];

  return
end

