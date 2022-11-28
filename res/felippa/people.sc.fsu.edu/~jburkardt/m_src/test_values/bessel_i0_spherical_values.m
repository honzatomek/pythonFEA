function [ n_data, x, fx ] = bessel_i0_spherical_values ( n_data )

%*****************************************************************************80
%
%% bessel_i0_spherical_values() returns some values of the Spherical Bessel function i0.
%
%  Discussion:
%
%    In Mathematica, the function can be evaluated by:
%
%      Sqrt[Pi/(2*x)] * BesselI[1/2,x]
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    06 January 2007
%
%  Author:
%
%    John Burkardt
%
%  Reference:
%
%    Milton Abramowitz, Irene Stegun,
%    Handbook of Mathematical Functions,
%    National Bureau of Standards, 1964,
%    LC: QA47.A34,
%    ISBN: 0-486-61272-4.
%
%    Stephen Wolfram,
%    The Mathematica Book,
%    Fourth Edition,
%    Wolfram Media / Cambridge University Press, 1999.
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
%    real X, the argument of the function.
%
%    real FX, the value of the function.
%
  n_max = 21;

  fx_vec = [ ...
    1.001667500198440E+00, ...
    1.006680012705470E+00, ...
    1.026880814507039E+00, ...
    1.061089303580402E+00, ...
    1.110132477734529E+00, ...
    1.175201193643801E+00, ...
    1.257884462843477E+00, ...
    1.360215358179667E+00, ...
    1.484729970750144E+00, ...
    1.634541271164267E+00, ...
    1.813430203923509E+00, ...
    2.025956895698133E+00, ...
    2.277595505698373E+00, ...
    2.574897010920645E+00, ...
    2.925685126512827E+00, ...
    3.339291642469967E+00, ...
    3.826838748926716E+00, ...
    4.401577467270101E+00, ...
    5.079293155726485E+00, ...
    5.878791279137455E+00, ...
    6.822479299281938E+00 ];

  x_vec = [ ...
     0.1E+00, ...
     0.2E+00, ...
     0.4E+00, ...
     0.6E+00, ...
     0.8E+00, ...
     1.0E+00, ...
     1.2E+00, ...
     1.4E+00, ...
     1.6E+00, ...
     1.8E+00, ...
     2.0E+00, ...
     2.2E+00, ...
     2.4E+00, ...
     2.6E+00, ...
     2.8E+00, ...
     3.0E+00, ...
     3.2E+00, ...
     3.4E+00, ...
     3.6E+00, ...
     3.8E+00, ...
     4.0E+00 ];

  if ( n_data < 0 )
    n_data = 0;
  end

  n_data = n_data + 1;

  if ( n_max < n_data )
    n_data = 0;
    x = 0.0;
    fx = 0.0;
  else
    x = x_vec(n_data);
    fx = fx_vec(n_data);
  end

  return
end