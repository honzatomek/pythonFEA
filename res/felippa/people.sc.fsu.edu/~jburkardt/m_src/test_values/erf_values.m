function [ n_data, x, fx ] = erf_values ( n_data )

%*****************************************************************************80
%
%% erf_values() returns some values of the ERF or "error" function.
%
%  Discussion:
%
%    The error function is defined by:
%
%      ERF(X) = ( 2 / sqrt ( PI ) * integral ( 0 <= T <= X ) exp ( - T^2 ) dT
%
%    In Mathematica, the function can be evaluated by:
%
%      Erf[x]
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
%    Milton Abramowitz, Irene Stegun,
%    Handbook of Mathematical Functions,
%    National Bureau of Standards, 1964,
%    ISBN: 0-486-61272-4,
%    LC: QA47.A34.
%
%    Stephen Wolfram,
%    The Mathematica Book,
%    Fourth Edition,
%    Cambridge University Press, 1999,
%    ISBN: 0-521-64314-7,
%    LC: QA76.95.W65.
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
     0.0000000000000000E+00, ...
     0.1124629160182849E+00, ...
     0.2227025892104785E+00, ...
     0.3286267594591274E+00, ...
     0.4283923550466685E+00, ...
     0.5204998778130465E+00, ...
     0.6038560908479259E+00, ...
     0.6778011938374185E+00, ...
     0.7421009647076605E+00, ...
     0.7969082124228321E+00, ...
     0.8427007929497149E+00, ...
     0.8802050695740817E+00, ...
     0.9103139782296354E+00, ...
     0.9340079449406524E+00, ...
     0.9522851197626488E+00, ...
     0.9661051464753107E+00, ...
     0.9763483833446440E+00, ...
     0.9837904585907746E+00, ...
     0.9890905016357307E+00, ...
     0.9927904292352575E+00, ...
     0.9953222650189527E+00 ]; 

  x_vec = [ ...
     0.0E+00, ... 
     0.1E+00, ... 
     0.2E+00, ... 
     0.3E+00, ... 
     0.4E+00, ... 
     0.5E+00, ... 
     0.6E+00, ... 
     0.7E+00, ... 
     0.8E+00, ... 
     0.9E+00, ... 
     1.0E+00, ... 
     1.1E+00, ... 
     1.2E+00, ... 
     1.3E+00, ... 
     1.4E+00, ... 
     1.5E+00, ... 
     1.6E+00, ... 
     1.7E+00, ... 
     1.8E+00, ... 
     1.9E+00, ... 
     2.0E+00 ];

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