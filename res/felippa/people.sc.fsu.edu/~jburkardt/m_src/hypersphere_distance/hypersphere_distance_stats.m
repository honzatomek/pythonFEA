function [ mu, var ] = hypersphere_distance_stats ( m, n )

%*****************************************************************************80
%
%% hypersphere_distance_stats estimates hypersphere distance statistics.
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
%    integer N, the number of sample points to use.
%
%  Output:
%
%    real MU, VAR, the estimated mean and variance of the
%    distance between two random points on the unit sphere.
%
  t = zeros ( n, 1 );
  for i = 1 : n
    p = hypersphere_unit_sample ( m );
    q = hypersphere_unit_sample ( m );
    t(i) = norm ( p - q );
  end

  mu = sum ( t(1:n) ) / n;
  if ( 1 < n )
    var = sum ( ( t(1:n) - mu ) .^ 2 ) / ( n - 1 );
  else
    var = 0.0;
  end

  return
end
