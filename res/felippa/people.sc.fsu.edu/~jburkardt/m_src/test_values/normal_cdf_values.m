function [ n_data, mu, sigma, x, fx ] = normal_cdf_values ( n_data )

%*****************************************************************************80
%
%% normal_cdf_values() returns some values of the Normal CDF.
%
%  Discussion:
%
%    In Mathematica, the function can be evaluated by:
%
%      Needs["Statistics`ContinuousDistributions`"]
%      dist = NormalDistribution [ mu, sigma ]
%      CDF [ dist, x ]
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
%    real MU, the mean of the distribution.
%
%    real SIGMA, the standard deviation of the distribution.
%
%    real X, the argument of the function.
%
%    real FX, the value of the function.
%
  n_max = 12;

  fx_vec = [ ...
     0.5000000000000000E+00, ...
     0.9772498680518208E+00, ...
     0.9999683287581669E+00, ...
     0.9999999990134124E+00, ...
     0.6914624612740131E+00, ...
     0.6305586598182364E+00, ...
     0.5987063256829237E+00, ...
     0.5792597094391030E+00, ...
     0.6914624612740131E+00, ...
     0.5000000000000000E+00, ...
     0.3085375387259869E+00, ...
     0.1586552539314571E+00 ];

  mu_vec = [ ...
     0.1000000000000000E+01, ...  
     0.1000000000000000E+01, ...  
     0.1000000000000000E+01, ...  
     0.1000000000000000E+01, ...  
     0.1000000000000000E+01, ...  
     0.1000000000000000E+01, ...  
     0.1000000000000000E+01, ...  
     0.1000000000000000E+01, ...  
     0.2000000000000000E+01, ...  
     0.3000000000000000E+01, ...  
     0.4000000000000000E+01, ...  
     0.5000000000000000E+01 ]; 

  sigma_vec = [ ...
     0.5000000000000000E+00, ...  
     0.5000000000000000E+00, ...
     0.5000000000000000E+00, ...
     0.5000000000000000E+00, ...
     0.2000000000000000E+01, ...
     0.3000000000000000E+01, ...
     0.4000000000000000E+01, ...
     0.5000000000000000E+01, ...
     0.2000000000000000E+01, ...
     0.2000000000000000E+01, ...
     0.2000000000000000E+01, ...
     0.2000000000000000E+01 ];

  x_vec = [ ...
     0.1000000000000000E+01, ...  
     0.2000000000000000E+01, ...  
     0.3000000000000000E+01, ...  
     0.4000000000000000E+01, ...  
     0.2000000000000000E+01, ...  
     0.2000000000000000E+01, ...  
     0.2000000000000000E+01, ...  
     0.2000000000000000E+01, ...  
     0.3000000000000000E+01, ...  
     0.3000000000000000E+01, ...  
     0.3000000000000000E+01, ...  
     0.3000000000000000E+01 ];

  if ( n_data < 0 )
    n_data = 0;
  end

  n_data = n_data + 1;

  if ( n_max < n_data )
    n_data = 0;
    mu = 0.0;
    sigma = 0.0;
    x = 0.0;
    fx = 0.0;
  else
    mu = mu_vec(n_data);
    sigma = sigma_vec(n_data);
    x = x_vec(n_data);
    fx = fx_vec(n_data);
  end

  return
end
