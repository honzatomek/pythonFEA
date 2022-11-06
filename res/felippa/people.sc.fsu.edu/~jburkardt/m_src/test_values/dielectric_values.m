function [ n_data, tc, p, eps ] = dielectric_values ( n_data )

%*****************************************************************************80
%
%% dielectric_values() returns some values of the static dielectric constant.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    16 September 2004
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
%      for Vapor and Liquid States of Water in SI Units,
%    Hemisphere Publishing Corporation, Washington, 1984,
%    TJ270.H3, page 266.
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
%    real P, the pressure, in bar.
%
%    real EPS, the dielectric constant, dimensionless.
%
  n_max = 15;

  eps_vec = [ ...
      88.29E+00, ...
      90.07E+00, ...
      92.02E+00, ...
      95.14E+00, ...
     100.77E+00, ...
      78.85E+00, ...
      70.27E+00, ...
      62.60E+00, ...
      55.78E+00, ...
      44.31E+00, ...
      35.11E+00, ...
      20.40E+00, ...
       1.17E+00, ...
       1.11E+00, ...
       1.08E+00 ];

  p_vec = [ ...
      100.0E+00, ...
      500.0E+00, ...
     1000.0E+00, ...
     2000.0E+00, ...
     5000.0E+00, ...
      100.0E+00, ...
      100.0E+00, ...
      100.0E+00, ...
      100.0E+00, ...
      100.0E+00, ...
      100.0E+00, ...
      100.0E+00, ...
      100.0E+00, ...
      100.0E+00, ...
      100.0E+00 ];

  tc_vec = [ ...
       0.0E+00, ...
       0.0E+00, ...
       0.0E+00, ...
       0.0E+00, ...
       0.0E+00, ...
      25.0E+00, ...
      50.0E+00, ...
      75.0E+00, ...
     100.0E+00, ...
     150.0E+00, ...
     200.0E+00, ...
     300.0E+00, ...
     400.0E+00, ...
     500.0E+00, ...
     600.0E+00 ];

  if ( n_data < 0 )
    n_data = 0;
  end

  n_data = n_data + 1;

  if ( n_max < n_data )
    n_data = 0;
    tc = 0.0;
    p = 0.0;
    eps = 0.0;
  else
    tc = tc_vec(n_data);
    p = p_vec(n_data);
    eps = eps_vec(n_data);
  end

  return
end
