function dist = polygon_point_dist ( n, v, p )

%*****************************************************************************80
%
%% polygon_point_dist(): distance ( polygon, point ).
%
%  Discussion:
%
%    Thanks to Stefano Zappacosta for pointing out that the arguments
%    passed to SEGMENT_POINT_DIST needed to be transposed,
%    27 June 2006.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    27 June 2006
%
%  Author:
%
%    John Burkardt
%
%  Input:
%
%    integer N, the number of vertices.
%
%    real V(2,N), the vertices of the polygon.
%
%    real P(2,1), the point to be checked.
%
%  Output:
%
%    real DIST, the distance from the point to the polygon.
%

%
%  Find the distance to each of the line segments.
%
  dist = inf;

  for j = 1 : n

    jp1 = j + 1;
    if ( n < jp1 )
      jp1 = jp1 - n;
    end

    dist2 = segment_point_dist ( v(1:2,j), v(1:2,jp1), p );

    if ( dist2 < dist )
      dist = dist2;
    end

  end

  return
end
