function variance = normal_ms_variance ( mu, sigma )

%*****************************************************************************80
%
%% normal_ms_variance() returns the variance of the Normal PDF.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    17 September 2004
%
%  Author:
%
%    John Burkardt
%
%  Input:
%
%    real MU, SIGMA, the parameters of the PDF.
%    0.0 < SIGMA.
%
%  Output:
%
%    real VARIANCE, the variance of the PDF.
%
  variance = sigma * sigma;

  return
end
