function [ m0, m1, m2, t0, y0 ] = three_body_parameters ( )

%*****************************************************************************80
%
%% three_body_parameters returns parameters for the three body ODE.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    11 November 2020
%
%  Author:
%
%    John Burkardt
%
%  Output:
%
%    real M0, M1, M2: the masses of the three bodies.
%
%    real T0: the initial time.
%
%    real Y0(12): the initial condition at time T0.
%
  m0 = 5.0;
  m1 = 3.0;
  m2 = 4.0;

  t0 = 0.0;
  y0 = [ ...
     1.0; -1.0;  0.0;  0.0; ...
     1.0;  3.0;  0.0;  0.0; ...
    -2.0; -1.0;  0.0;  0.0 ];

  return
end

