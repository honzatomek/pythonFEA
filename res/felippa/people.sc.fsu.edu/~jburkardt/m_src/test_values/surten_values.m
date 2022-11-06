function [ n_data, tc, sigma ] = surten_values ( n_data )

%*****************************************************************************80
%
%% surten_values() returns some values of the surface tension.
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
%    TJ270.H3, pages 267.
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
%    real TC, the temperature, in degrees Celsius.
%
%    real SIGMA, the surface tension,
%    in Pascal * m = Newton / m.
%
  n_max = 14;

  sigma_vec = [ ...
     74.22E+00, ...
     72.74E+00, ...
     71.20E+00, ...
     69.60E+00, ...
     67.95E+00, ...
     58.92E+00, ...
     48.75E+00, ...
     37.68E+00, ...
     26.05E+00, ...
     14.37E+00, ...
      8.78E+00, ...
      3.67E+00, ...
      0.40E+00, ...
      0.00E+00 ];

  tc_vec = [ ...
      10.000E+00, ...
      20.000E+00, ...
      30.000E+00, ...
      40.000E+00, ...
      50.000E+00, ...
     100.000E+00, ...
     150.000E+00, ...
     200.000E+00, ...
     250.000E+00, ...
     300.000E+00, ...
     325.000E+00, ...
     350.000E+00, ...
     370.000E+00, ...
     373.976E+00 ];

  if ( n_data < 0 )
    n_data = 0;
  end

  n_data = n_data + 1;

  if ( n_max < n_data )
    n_data = 0;
    tc = 0.0;
    sigma = 0.0;
  else
    tc = tc_vec(n_data);
    sigma = sigma_vec(n_data);
  end

  return
end
