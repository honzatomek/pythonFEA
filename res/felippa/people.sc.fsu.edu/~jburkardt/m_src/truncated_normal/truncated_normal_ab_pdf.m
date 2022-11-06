function pdf = truncated_normal_ab_pdf ( x, mu, sigma, a, b )

%*****************************************************************************80
%
%% truncated_normal_ab_pdf() evaluates the truncated Normal PDF.
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
%    real X, the argument of the PDF.
%
%    real MU, SIGMA, the mean and standard deviation of the
%    parent Normal distribution.
%
%    real A, B, the lower and upper truncation limits.
%
%  Output:
%
%    real PDF, the value of the PDF.
%
  if ( x <= a )
  
    pdf = 0.0;
    
  elseif ( x <= b )
  
    alpha = ( a - mu ) / sigma;
    beta = ( b - mu ) / sigma;
    xi = ( x - mu ) / sigma;

    alpha_cdf = normal_01_cdf ( alpha );
    beta_cdf = normal_01_cdf ( beta );
    xi_pdf = normal_01_pdf ( xi );

    pdf = xi_pdf / ( beta_cdf - alpha_cdf ) / sigma;
    
  else
  
    pdf = 0.0;
    
  end
  
  return
end
