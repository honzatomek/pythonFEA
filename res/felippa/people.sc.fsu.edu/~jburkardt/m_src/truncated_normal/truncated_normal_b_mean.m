function mean = truncated_normal_b_mean ( mu, sigma, b )

%*****************************************************************************80
%
%% truncated_normal_b_mean() returns the mean of the upper truncated Normal PDF.
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
%    real MEAN, the mean of the PDF.
%
  beta = ( b - mu ) / sigma;

  beta_cdf = normal_01_cdf ( beta );

  beta_pdf = normal_01_pdf ( beta );

  mean = mu - sigma * beta_pdf / beta_cdf;

  return
end
