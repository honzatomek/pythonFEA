function [ n_data, p, tc ] = tsat_values ( n_data )

%*****************************************************************************80
%
%% tsat_values() returns some values of the saturation temperature.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    19 September 2004
%
%  Author:
%
%    John Burkardt
%
%  Reference:
%
%    Lester Haar, John Gallagher and George Kell,
%    NBS/NRC Steam Tables:
%    Thermodynamic and Transport Properties and Computer Programs
%    for Vapor and Liquid States of Water in SI Units,
%    Hemisphere Publishing Corporation, Washington, 1984,
%    TJ270.H3, pages 16-22.
%
%  Input:
%
%    integer N_DATA.  The user sets N_DATA to 0 before the first call.  
%    Thereafter, it should simply be the value returned by the previous call.
%
%  Output:
%
%    integer N_DATA.  On each call, the routine increments N_DATA by 1, and
%    returns the corresponding data; when there is no more data, the
%    output value of N_DATA will be 0 again.
%
%    real P, the pressure, in bar.
%
%    real TC, the saturation temperature, in
%    degrees Celsius.
%
  n_max = 20;

  p_vec = [ ...
       0.0061173, ...
       0.012, ...
       0.025, ...
       0.055, ...
       0.080, ...
       0.110, ...
       0.160, ...
       0.250, ...
       0.500, ...
       0.750, ...
       1.000, ...
       1.500, ...
       2.000, ...
       5.000, ...
      10.000, ...
      20.000, ...
      50.000, ...
     100.000, ...
     200.000, ...
     220.550 ];

  tc_vec = [ ...
       0.010, ...
       9.655, ...
      21.080, ...
      34.589, ...
      41.518, ...
      47.695, ...
      55.327, ...
      64.980, ...
      81.339, ...
      91.783, ...
      99.632, ...
     111.378, ...
     120.443, ...
     151.866, ...
     179.916, ...
     212.417, ...
     263.977, ...
     311.031, ...
     365.800, ...
     373.976 ];

  if ( n_data < 0 )
    n_data = 0;
  end

  n_data = n_data + 1;

  if ( n_max < n_data )
    n_data = 0;
    p = 0.0;
    tc = 0.0;
  else
    p = p_vec(n_data);
    tc = tc_vec(n_data);
  end

  return
end