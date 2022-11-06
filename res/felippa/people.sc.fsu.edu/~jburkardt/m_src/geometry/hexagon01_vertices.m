function v = hexagon01_vertices ( )

%*****************************************************************************80
%
%% hexagon_vertices_2d() returns the vertices of the unit hexagon.
%
%  Discussion:
%
%    The unit hexagon has maximum radius 1, and is the hull of the points
%
%      (   1,              0 ),
%      (   0.5,   sqrt (3)/2 ),
%      ( - 0.5,   sqrt (3)/2 ),
%      ( - 1,              0 ),
%      ( - 0.5, - sqrt (3)/2 ),
%      (   0.5, - sqrt (3)/2 ).
%
%      120_____60
%        /     \
%    180/       \0
%       \       /
%        \_____/
%      240     300
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    08 May 2005
%
%  Author:
%
%    John Burkardt
%
%  Output:
%
%    real V(2,6), the coordinates of the vertices.
%
  a = sqrt ( 3.0 ) / 2.0;

  v = [ 1.0, 0.5, -0.5, -1.0, -0.5, 0.5;
        0.0, a,    a,    0.0, -a,   -a ];

  return
end
