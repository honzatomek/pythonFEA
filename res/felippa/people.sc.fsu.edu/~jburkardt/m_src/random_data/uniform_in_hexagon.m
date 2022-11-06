function xy = uniform_in_hexagon ( n )

%*****************************************************************************80
%
%% uniform_in_hexagon() samples uniformly from the regular unit hexagon.
%
%  Discussion:
%
%    The unit hexagon has center (0,0) and "radius" 1.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license. 
%
%  Modified:
%
%    09 April 2022
%
%  Author:
%
%    John Burkardt
%
%  Input:
%
%    integer N, the number of points.
%
%  Output:
%
%    real XY(N,2), the points.
%

%
%  Set basis vectors for each rhombus.
%
  v = [ ...
    -1.0, 0.0; ...
     0.5, sqrt(3.0)/2.0; ...
     0.5, -sqrt(3.0)/2.0; ...
    -1.0, 0.0 ];
%
%  Assign each point randomly to one of the rhombuses.
%
  rh = randi ( [ 1, 3 ], n, 1 );

  i = find ( rh == 1 );
  j = find ( rh == 2 );
  k = find ( rh == 3 );
%
%  Set barycentric coordinates of each point in its rhombus.
%
  xy = rand ( n,2 );
%
%  Convert to physical coordinates.
%
  xy(i,1:2) = xy(i,1:2) * v(1:2,1:2);
  xy(j,1:2) = xy(j,1:2) * v(2:3,1:2);
  xy(k,1:2) = xy(k,1:2) * v(3:4,1:2);

  return
end
