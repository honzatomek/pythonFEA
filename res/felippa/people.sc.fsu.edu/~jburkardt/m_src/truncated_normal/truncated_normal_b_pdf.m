function pdf = truncated_normal_b_pdf ( x, mu, sigma, b )

%*****************************************************************************80
%
%% truncated_normal_b_pdf() evaluates the upper truncated Normal PDF.
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
%    real B, the upper truncation limit.
%
%  Output:
%
%    real PDF, the value of the PDF.
%
  if ( x <= b )
  
    beta = ( b - mu ) / sigma;
    xi = ( x - mu ) / sigma;

    beta_cdf = normal_01_cdf ( beta );
    xi_pdf = normal_01_pdf ( xi );

    pdf = xi_pdf / beta_cdf / sigma;

  else
  
    pdf = 0.0;
    
  end
  
  return
end
