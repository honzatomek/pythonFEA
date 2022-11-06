function x = truncated_normal_a_sample ( mu, sigma, a )

%*****************************************************************************80
%
%% truncated_normal_a_sample() samples the lower truncated Normal PDF.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    20 August 2013
%
%  Author:
%
%    John Burkardt
%
%  Input:
%
%    real MU, SIGMA, the mean and standard deviation of the
%    parent Normal distribution.
%
%    real A, the lower truncation limit.
%
%  Output:
%
%    real X, a sample of the PDF.
%
  alpha = ( a - mu ) / sigma;

  alpha_cdf = normal_01_cdf ( alpha );
  beta_cdf = 1.0;

  u = rand ( 1, 1 );
  xi_cdf = alpha_cdf + u * ( beta_cdf - alpha_cdf );
  xi = normal_01_cdf_inv ( xi_cdf );

  x = mu + sigma * xi;

  return
end
