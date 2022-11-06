function [ d_mean, d_var, d_min, d_max ] = polygon_distance_stats ( n, nv, v )

%*****************************************************************************80
%
%% polygon_distance_stats() estimates polygon distance statistics.
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
%    integer N, the number of sample points to use.
%
%    integer NV, the number of vertices.
%
%    real V(2,NV), the vertices.
%
%  Output:
%
%    real D_MEAN, D_VAR, D_MIN, D_MAX, the mean, variance,
%    minimum and maximum of the sample values.
%
  p1 = polygon_sample ( nv, v, n );
  p2 = polygon_sample ( nv, v, n ); 
  d = vecnorm ( p1 - p2 );
  d_mean = mean ( d );
  d_var = var ( d );
  d_min = min ( d );
  d_max = max ( d );

  return
end
