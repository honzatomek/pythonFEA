function hypersphere_distance_pdf ( m )

%*****************************************************************************80
%
%% hypersphere_distance_pdf plots the PDF for the hypersphere distance problem.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    29 September 2018
%
%  Author:
%
%    John Burkardt
%
%  Reference:
%
%    Panagiotis Siridopoulos,
%    The N-Sphere Chord Length Distribution,
%    ARXIV,
%    https://arxiv.org/pdf/1411.5639.pdf
%
%  Input:
%
%    integer M, the spatial dimension.
%
  d = linspace ( 0.0, 2.0, 101 );

  pdf = d / beta ( ( m - 1 ) / 2, 0.5 ) .* ( d.^2 - 0.25 * d.^4 ) .^ ( ( m - 3 ) / 2 );

  plot ( d, pdf, 'r-', 'linewidth', 2 );
  grid ( 'on' );
  xlabel ( '<-- Distance -->' );
  ylabel ( '<-- Probability -->' );
  title ( 'PDF for pairwise distance of random points on unit hypersphere' );

  return
end

