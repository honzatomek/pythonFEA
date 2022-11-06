function [ n_data, x, fx ] = cin_values ( n_data )

%*****************************************************************************80
%
%% cin_values() returns some values of the alternate cosine integral function.
%
%  Discussion:
%
%    The alternate cosine integral is defined by
%
%      CIN(X) = gamma + log(X) + integral ( 0 <= T <= X ) ( cos ( T ) - 1 ) / T  dT
%
%    In Mathematica, the function can be evaluated by:
%
%      EulerGamma + Log[x] - CosIntegral[x]
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
     0.6185256314820045E-01, ...
     0.8866074809482194E-01, ...
     0.1200260139539026E+00, ...
     0.1557934976348559E+00, ...
     0.1957873187759337E+00, ...
     0.2398117420005647E+00, ...
     0.3390780388012470E+00, ...
     0.4516813164280685E+00, ...
     0.5754867772153906E+00, ...
     0.7081912003853150E+00, ...
     0.8473820166866132E+00, ...
     0.1207635200410304E+01, ...
     0.1556198167561642E+01, ...
     0.1862107181909382E+01, ...
     0.2104491723908354E+01, ...
     0.2274784183779546E+01 ];

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
