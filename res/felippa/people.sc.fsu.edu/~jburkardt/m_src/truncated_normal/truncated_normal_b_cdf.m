function cdf = truncated_normal_b_cdf ( x, mu, sigma, b )

%*****************************************************************************80
%
%% truncated_normal_b_cdf() evaluates the upper truncated Normal CDF.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    24 January 2017
%
%  Author:
%
%    John Burkardt
%
%  Input:
%
%    real X, the argument of the CDF.
%
%    real MU, SIGMA, the mean and standard deviation of the
%    parent Normal distribution.
%
%    real B, the upper truncation limit.
%
%  Output:
%
%    real CDF, the value of the CDF.
%
  if ( x < b )
  
    beta = ( b - mu ) / sigma;
    xi = ( x - mu ) / sigma;

    beta_cdf = normal_01_cdf ( beta );
    xi_cdf = normal_01_cdf ( xi );

    cdf = xi_cdf / beta_cdf;

  else
  
    cdf = 1.0;
    
  end
  
  return
end
