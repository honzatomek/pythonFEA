function [ n_data, nu, x, fx ] = bessel_yn_values ( n_data )

%*****************************************************************************80
%
%% bessel_yn_values() returns some values of the Yn Bessel function.
%
%  Discussion:
%
%    In Mathematica, the function can be evaluated by:
%
%      BesselY[n,x]
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    26 August 2004
%
%  Author:
%
%    John Burkardt
%
%  Reference:
%
%    Milton Abramowitz and Irene Stegun,
%    Handbook of Mathematical Functions,
%    US Department of Commerce, 1964.
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
%    integer NU, the order of the function.
%
%    real X, the argument of the function.
%
%    real FX, the value of the function.
%
  n_max = 20;

  fx_vec = [ ...
     -0.1650682606816254E+01, ...
     -0.6174081041906827E+00, ...
      0.3676628826055245E+00, ...
     -0.5868082442208615E-02, ...
      0.9579316872759649E-01, ...
     -0.2604058666258122E+03, ...
     -0.9935989128481975E+01, ...
     -0.4536948224911019E+00, ...
      0.1354030476893623E+00, ...
     -0.7854841391308165E-01, ...
     -0.1216180142786892E+09, ...
     -0.1291845422080393E+06, ...
     -0.2512911009561010E+02, ...
     -0.3598141521834027E+00, ...
      0.5723897182053514E-02, ...
     -0.4113970314835505E+23, ...
     -0.4081651388998367E+17, ...
     -0.5933965296914321E+09, ...
     -0.1597483848269626E+04, ...
      0.1644263394811578E-01 ];

  nu_vec = [ ...
     2,  2,  2,  2, ...
     2,  5,  5,  5, ...
     5,  5, 10, 10, ...
    10, 10, 10, 20, ...
    20, 20, 20, 20 ];

  x_vec = [ ...
      1.0E+00, ... 
      2.0E+00, ...
      5.0E+00, ...
     10.0E+00, ...
     50.0E+00, ...
      1.0E+00, ...
      2.0E+00, ...
      5.0E+00, ...
     10.0E+00, ...
     50.0E+00, ...
      1.0E+00, ...
      2.0E+00, ...
      5.0E+00, ...
     10.0E+00, ...
     50.0E+00, ...
      1.0E+00, ...
      2.0E+00, ...
      5.0E+00, ...
     10.0E+00, ...
     50.0E+00 ];

  if ( n_data < 0 )
    n_data = 0;
  end

  n_data = n_data + 1;

  if ( n_max < n_data )
    n_data = 0;
    nu = 0;
    x = 0.0;
    fx = 0.0;
  else
    nu = nu_vec(n_data);
    x = x_vec(n_data);
    fx = fx_vec(n_data);
  end

  return
end
