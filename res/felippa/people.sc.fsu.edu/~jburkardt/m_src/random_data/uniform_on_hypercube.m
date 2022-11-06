function x = uniform_on_hypercube ( n, d )

%*****************************************************************************80
%
%% uniform_on_hypercube() returns random points on the surface of the unit hypercube.
%
%  Discussion:
%
%    The cube is assumed to have center C at the origin, and radius 1.
%
%    Any point on the surface of the cube is a D-dimensional vector 
%    whose entries are between -1 and +1, and for which at least one value 
%    has norm 1.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    11 April 2022
%
%  Author:
%
%    John Burkardt
%
%  Input:
%
%    integer N, the number of points.
%
%    integer D, the spatial dimension.
%
%  Output:
%
%    real X(N,D), the coordinates of N points, chosen uniformly at
%    random from the surface of the unit D-dimensional hypercube.
%

%
%  Choose random points within the cube of radius 1.
%
  x = 2.0 * rand ( n, d ) - 1.0;
%
%  For each point, select a coordinate J at random, and set it to +1 or -1.
%
  j = randi ( [ 1, d ], 1, n );
%
%  Randomly set coordinate J to -1 or +1.
%
  i = 1 : n;
  k = i + ( j - 1 ) * d;
  s = 2.0 * randi ( [ 0, 1 ], 1, n ) - 1.0;
  x(k) = s;

  return
end
