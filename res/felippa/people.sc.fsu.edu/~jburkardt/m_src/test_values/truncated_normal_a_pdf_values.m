function [ n_data, mu, sigma, a, x, fx ] = ...
  truncated_normal_a_pdf_values ( n_data )

%*****************************************************************************80
%
%% truncated_normal_a_pdf_values() returns values of the lower Truncated Normal AB PDF.
%
%  Discussion:
%
%    The Normal distribution, with mean Mu and standard deviation Sigma,
%    is truncated to the interval [A,+oo).
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    13 September 2013
%
%  Author:
%
%    John Burkardt
%
%  Reference:
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
%    real A, the lower truncation limit.
%
%    real X, the argument of the function.
%
%    real FX, the value of the function.
%
  n_max = 11;

  a_vec = [ ...
     50.0, ...
     50.0, ...
     50.0, ...
     50.0, ...
     50.0, ...
     50.0, ...
     50.0, ...
     50.0, ...
     50.0, ...
     50.0, ...
     50.0 ];

  fx_vec = [ ...
     0.01507373507401876, ...
     0.01551417047139894, ...
     0.01586560931024694, ...
     0.01612150073158793, ...
     0.01627701240029317, ...
     0.01632918226724295, ...
     0.01627701240029317, ...
     0.01612150073158793, ...
     0.01586560931024694, ...
     0.01551417047139894, ...
     0.01507373507401876 ];

  mu_vec = [ ...
     100.0, ...
     100.0, ...
     100.0, ...
     100.0, ...
     100.0, ...
     100.0, ...
     100.0, ...
     100.0, ...
     100.0, ...
     100.0, ...
     100.0 ]; 

  sigma_vec = [ ...
    25.0, ...
    25.0, ...
    25.0, ...
    25.0, ...
    25.0, ...
    25.0, ...
    25.0, ...
    25.0, ...
    25.0, ...
    25.0, ...
    25.0  ];

  x_vec = [ ...
     90.0, ...
     92.0, ...
     94.0, ...
     96.0, ...
     98.0, ...
    100.0, ...
    102.0, ...
    104.0, ...
    106.0, ...
    108.0, ...
    110.0 ];

  if ( n_data < 0 )
    n_data = 0;
  end

  n_data = n_data + 1;

  if ( n_max < n_data )
    n_data = 0;
    a = 0.0;
    mu = 0.0;
    sigma = 0.0;
    x = 0.0;
    fx = 0.0;
  else
    a = a_vec(n_data);
    mu = mu_vec(n_data);
    sigma = sigma_vec(n_data);
    x = x_vec(n_data);
    fx = fx_vec(n_data);
  end

  return
end
