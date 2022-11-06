function mean = truncated_normal_a_mean ( mu, sigma, a )

%*****************************************************************************80
%
%% truncated_normal_a_mean() returns the mean of the lower truncated Normal PDF.
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
%    real MEAN, the mean of the PDF.
%
  alpha = ( a - mu ) / sigma;

  alpha_cdf = normal_01_cdf ( alpha );

  alpha_pdf = normal_01_pdf ( alpha );

  mean = mu + sigma * alpha_pdf / ( 1.0 - alpha_cdf );

  return
end
