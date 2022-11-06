function [ n_data, x, fx ] = si_values ( n_data )

%*****************************************************************************80
%
%% si_values() returns some values of the sine integral function.
%
%  Discussion:
%
%    SI(X) = integral ( 0 <= T <= X ) sin ( T ) / T dt
%
%    In Mathematica, the function can be evaluated by:
%
%      SinIntegral[x]
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
%    real X, the argument of the function.
%
%    real FX, the value of the function.
%
  n_max = 16;

  fx_vec = [ ...
     0.4931074180430667E+00, ...
     0.5881288096080801E+00, ...
     0.6812222391166113E+00, ...
     0.7720957854819966E+00, ...
     0.8604707107452929E+00, ...
     0.9460830703671830E+00, ...
     0.1108047199013719E+01, ...
     0.1256226732779218E+01, ...
     0.1389180485870438E+01, ...
     0.1505816780255579E+01, ...
     0.1605412976802695E+01, ...
     0.1778520173443827E+01, ...
     0.1848652527999468E+01, ...
     0.1833125398665997E+01, ...
     0.1758203138949053E+01, ...
     0.1654140414379244E+01 ];

  x_vec = [ ...
      0.5E+00, ...
      0.6E+00, ...
      0.7E+00, ...
      0.8E+00, ...
      0.9E+00, ...
      1.0E+00, ...
      1.2E+00, ...
      1.4E+00, ...
      1.6E+00, ...
      1.8E+00, ...
      2.0E+00, ...
      2.5E+00, ...
      3.0E+00, ...
      3.5E+00, ...
      4.0E+00, ...
      4.5E+00 ];

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
