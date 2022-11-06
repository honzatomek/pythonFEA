function pdf = normal_ms_pdf ( x, mu, sigma )

%*****************************************************************************80
%
%% normal_ms_pdf() evaluates the Normal PDF.
%
%  Discussion:
%
%    The normal PDF is also known as the Gaussian PDF.
%
%  Formula:
%
%    PDF(X)(MU,SIGMA) = EXP ( - 0.5 * ( ( X - MU ) / SIGMA )^2 ) 
%      / SQRT ( 2 * PI * SIGMA^2 )
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    03 January 2011
%
%  Author:
%
%    John Burkardt
%
%  Input:
%
%    real X, the argument of the PDF.
%
%    real MU, SIGMA, the parameters of the PDF.
%    0.0 < SIGMA.
%
%  Output:
%
%    real PDF(), the value of the PDF.
%
  pdf = exp ( - 0.5 * ( ( x - mu ) / sigma ).^2 )  / sqrt ( 2.0 * pi * sigma^2 );

  return
end
