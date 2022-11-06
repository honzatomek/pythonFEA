function cdf = truncated_normal_ab_cdf ( x, mu, sigma, a, b )

%*****************************************************************************80
%
%% truncated_normal_ab_cdf() evaluates the truncated Normal CDF.
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
%    real A, B, the lower and upper truncation limits.
%
%  Output:
%
%    real CDF, the value of the CDF.
%
  if ( x <= a )
  
    cdf = 0.0;
  
  elseif ( x <= b )
 
    alpha = ( a - mu ) / sigma;
    beta = ( b - mu ) / sigma;
    xi = ( x - mu ) / sigma;

    alpha_cdf = normal_01_cdf ( alpha );
    beta_cdf = normal_01_cdf ( beta );
    xi_cdf = normal_01_cdf ( xi );

    cdf = ( xi_cdf - alpha_cdf ) / ( beta_cdf - alpha_cdf );
    
  else
  
    cdf = 1.0;
    
  end

  return
end
