function polygon_distance_histogram ( n, nv, v )

%*****************************************************************************80
%
%% polygon_distance_histogram() histograms polygon distance statistics.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    02 June 2019
%
%  Author:
%
%    John Burkardt
%
%  Input:
%
%    integer N, the number of samples to use.
%
%    integer NV, the number of vertices.
%
%    real V(2,NV), the vertices.
%
  p1 = polygon_sample ( nv, v, n );
  p2 = polygon_sample ( nv, v, n ); 
  d = vecnorm ( p1 - p2 );

  bins = 20;
  histogram ( d, bins, 'Normalization', 'pdf' );
  grid ( 'on' );
  xlabel ( '<-- Distance -->' );
  ylabel ( '<-- Frequency -->' );
  title ( 'Distance between a pair of random points in a polygon' );
  pause ( 5 );

  return
end
