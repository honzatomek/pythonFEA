function [ n_data, x, fx ] = erf_values ( n_data )

%*****************************************************************************80
%
%% erf_values() returns some values of the ERF or "error" function.
%
%  Definition:
%
%    ERF(X) = ( 2 / sqrt ( PI ) * integral ( 0 <= T <= X ) exp ( - T^2 ) dT
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    26 May 2004
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
%  Input:
%
%    integer N_DATA.  The user sets N_DATA to 0 before the first call.
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
    0.0000000000E+00, 0.1124629160E+00, 0.2227025892E+00, 0.3286267595E+00, ...
    0.4283923550E+00, 0.5204998778E+00, 0.6038560908E+00, 0.6778011938E+00, ...
    0.7421009647E+00, 0.7969082124E+00, 0.8427007929E+00, 0.8802050696E+00, ...
    0.9103139782E+00, 0.9340079449E+00, 0.9522851198E+00, 0.9661051465E+00, ...
    0.9763483833E+00, 0.9837904586E+00, 0.9890905016E+00, 0.9927904292E+00, ...
    0.9953222650E+00 ];
  x_vec = [ ...
    0.0E+00, 0.1E+00, 0.2E+00, 0.3E+00, ...
    0.4E+00, 0.5E+00, 0.6E+00, 0.7E+00, ...
    0.8E+00, 0.9E+00, 1.0E+00, 1.1E+00, ...
    1.2E+00, 1.3E+00, 1.4E+00, 1.5E+00, ...
    1.6E+00, 1.7E+00, 1.8E+00, 1.9E+00, ...
    2.0E+00 ];

  if ( n_data < 0 )
    n_data = 0;
  end

  n_data = n_data + 1;

  if ( n_max < n_data )
    n_data = 0;
    x = 0.0E+00;
    fx = 0.0E+00;
  else
    x = x_vec(n_data);
    fx = fx_vec(n_data);
  end

  return
end
