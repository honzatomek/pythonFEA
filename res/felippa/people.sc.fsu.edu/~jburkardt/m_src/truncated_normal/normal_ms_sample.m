function x = normal_ms_sample ( mu, sigma )

%*****************************************************************************80
%
%% normal_ms_sample() samples the Normal MS PDF.
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
%    real X, a sample of the PDF.
%
  x = mu + sigma * randn(1,1);

  return
end
