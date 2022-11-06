function x = truncated_normal_ab_cdf_inv ( cdf, mu, sigma, a, b )

%*****************************************************************************80
%
%% truncated_normal_ab_cdf_inv() inverts the truncated Normal CDF.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    14 August 2013
%
%  Author:
%
%    John Burkardt
%
%  Input:
%
%    real CDF, the value of the CDF.
%    0.0 <= CDF <= 1.0.
%
%    real MU, SIGMA, the mean and standard deviation of the
%    parent Normal distribution.
%
%    real A, B, the lower and upper truncation limits.
%
%  Output:
%
%    real X, the corresponding argument.
%
  if ( cdf < 0.0 || 1.0 < cdf )
    fprintf ( 1, '\n' );
    fprintf ( 1, 'TRUNCATED_NORMAL_AB_CDF_INV - Fatal error!\n' );
    fprintf ( 1, '  CDF < 0 or 1 < CDF.\n' );
    error ( 'TRUNCATED_NORMAL_AB_CDF_INV - Fatal error!' )
  end

  alpha = ( a - mu ) / sigma;
  beta = ( b - mu ) / sigma;

  alpha_cdf = normal_01_cdf ( alpha );
  beta_cdf = normal_01_cdf ( beta );

  xi_cdf = ( beta_cdf - alpha_cdf ) * cdf + alpha_cdf;
  xi = normal_01_cdf_inv ( xi_cdf );

  x = mu + sigma * xi;

  return
end
