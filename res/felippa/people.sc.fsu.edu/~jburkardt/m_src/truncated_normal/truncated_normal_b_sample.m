function x = truncated_normal_b_sample ( mu, sigma, b )

%*****************************************************************************80
%
%% truncated_normal_b_sample() samples the upper truncated Normal PDF.
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
%    real B, the upper truncation limit.
%
%  Output:
%
%    real X, a sample of the PDF.
%
  beta = ( b - mu ) / sigma;

  beta_cdf = normal_01_cdf ( beta );

  u = rand ( 1, 1 );
  xi_cdf =  u * beta_cdf;
  xi = normal_01_cdf_inv ( xi_cdf );

  x = mu + sigma * xi;

  return
end
