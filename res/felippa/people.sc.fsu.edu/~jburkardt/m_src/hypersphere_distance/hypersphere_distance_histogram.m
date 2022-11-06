function hypersphere_distance_histogram ( m, n )

%*****************************************************************************80
%
%% hypersphere_distance_histogram histograms hypersphere distance statistics.
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
%  Input:
%
%    integer M, the spatial dimension.
%
%    integer N, the number of samples to use.
%
  t = zeros ( n, 1 );
  for i = 1 : n
    p = hypersphere_unit_sample ( m );
    q = hypersphere_unit_sample ( m );
    t(i) = norm ( p - q );
  end

  bins = 20;
  histogram ( t, bins, 'Binlimits', [0.0,2.0], 'Normalization', 'pdf' );
  grid on
  xlabel ( '<-- Distance -->' )
  ylabel ( '<-- Frequency -->' )
  title ( 'Distance between a pair of random points on a unit hypersphere' )

  return
end
