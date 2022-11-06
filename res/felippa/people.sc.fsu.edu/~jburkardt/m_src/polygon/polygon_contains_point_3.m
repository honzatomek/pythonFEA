function inside = polygon_contains_point_3 ( n, v, p )

%*****************************************************************************80
%
%% polygon_contains_point_3() finds if a point is inside a simple polygon.
%
%  Discussion:
%
%    A simple polygon is one whose boundary never crosses itself.
%    The polygon does not need to be convex.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    05 May 2020
%
%  Author:
%
%    John Burkardt
%
%  Reference:
%
%    M Shimrat,
%    Position of Point Relative to Polygon,
%    ACM Algorithm 112,
%    Communications of the ACM,
%    Volume 5, Number 8, page 434, August 1962.
%
%  Input:
%
%    integer N, the number of nodes or vertices in the polygon.
%    N must be at least 3.
%
%    real V(2,N), the coordinates of the vertices of the polygon.
%
%    real P(2,1), the coordinates of the point to be tested.
%
%  Output:
%
%    logical INSIDE, is TRUE if the point is inside the polygon.
%
  inside = false;

  x1 = v(1,n);
  y1 = v(2,n);

  for i = 1 : n

    x2 = v(1,i);
    y2 = v(2,i);

    if ( ( y1 < p(2,1) & p(2,1) <= y2 ) | ( p(2,1) <= y1 & y2 < p(2,1) ) )
      if ( ( p(1,1) - x1 ) - ( p(2,1) - y1 ) * ( x2 - x1 ) / ( y2 - y1 ) < 0.0 )
        inside = ~ inside;
      end
    end

    x1 = x2;
    y1 = y2;

  end

  return
end
