function cdf = truncated_normal_a_cdf ( x, mu, sigma, a )

%*****************************************************************************80
%
%% truncated_normal_a_cdf() evaluates the lower truncated Normal CDF.
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
%    real A, the lower truncation limit.
%
%  Output:
%
%    real CDF, the value of the CDF.
%
  if ( x <= a )
  
    cdf = 0.0;
    
  else
  
    alpha = ( a - mu ) / sigma;
    xi = ( x - mu ) / sigma;

    alpha_cdf = normal_01_cdf ( alpha );
    xi_cdf = normal_01_cdf ( xi );

    cdf = ( xi_cdf - alpha_cdf ) / ( 1.0 - alpha_cdf );
    
  end
  
  return
end
