function point = p02_sample ( m, n )

%*****************************************************************************80
%
%% p02_sample() samples points from the region in problem 02.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    12 April 2009
%
%  Author:
%
%    John Burkardt
%
%  Input:
%
%    integer M, the spatial dimension.
%
%    integer N, the number of points.
%
%  Output:
%
%    real POINT(M,N), the coordinates 
%    of the points.
%
  center = [ 0.0, 0.0 ];
  r1 = 1.0;
  r2 = 0.4;
%
%  Choose uniform random angles between 0 and 2*Pi.
%
  angle = rand ( 1, n );

  angle(1,1:n) = 2.0 * pi * angle(1,1:n);
%
%  Choose uniform random radii between R2^2 and R1^2, then take square root.
%
  r = rand ( 1, n );
  r(1,1:n) = r2 * r2 + ( r1 * r1 - r2 * r2 ) * r(1,1:n);
  r(1,1:n) = sqrt ( r(1,1:n) );
%
%  Construct the uniformly random points in the circle of radius 1 
%  centered at 0.
%
  point(1,1:n) = center(1) + r(1,1:n) .* cos ( angle(1,1:n) );
  point(2,1:n) = center(2) + r(1,1:n) .* sin ( angle(1,1:n) );

  return
end
