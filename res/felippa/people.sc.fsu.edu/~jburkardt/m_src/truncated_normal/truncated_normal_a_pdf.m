function pdf = truncated_normal_a_pdf ( x, mu, sigma, a )

%*****************************************************************************80
%
%% truncated_normal_a_pdf() evaluates the lower truncated Normal PDF.
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
%    real MU, S, the mean and standard deviation of the
%    parent Normal distribution.
%
%    real A, the lower truncation limit.
%
%  Output:
%
%    real PDF, the value of the PDF.
%
  if ( x <= a )
  
    pdf = 0.0;
    
  else
  
    alpha = ( a - mu ) / sigma;
    xi = ( x - mu ) / sigma;

    alpha_cdf = normal_01_cdf ( alpha );
    xi_pdf = normal_01_pdf ( xi );

    pdf = xi_pdf / ( 1.0 - alpha_cdf ) / sigma;

  end
  
  return
end
