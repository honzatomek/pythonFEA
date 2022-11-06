function x = uniform_in_polygon ( n, nv, v )

%*****************************************************************************80
%
%% uniform_in_polygon() maps uniform points into a polygon.
%
%  Discussion:
%
%    If the polygon is regular, or convex, or at least star-shaped,
%    this routine will work.
%
%    This routine assumes that all points between the centroid and
%    any point on the boundary lie within the polygon.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license. 
%
%  Modified:
%
%    13 April 2022
%
%  Author:
%
%    John Burkardt
%
%  Input:
%
%    integer N, the number of points to create.
%
%    integer NV, the number of vertices.
%
%    real V(NV,2), the vertices of the polygon, listed in
%    clockwise or counterclockwise order.
%
%  Output:
%
%    real X(N,2), the points.
%
  x = zeros ( n, 2 );
%
%  Find the centroid.
%
  centroid = polygon_centroid ( nv, v );
%
%  Determine the areas of each triangle.
%
  for i = 1 : nv

    if ( i < nv )
      ip1 = i + 1;
    else
      ip1 = 1;
    end
    
    t(1:2,1) = v(i,1:2);
    t(1:2,2) = v(ip1,1:2);
    t(1:2,3) = centroid(1:2);
    
    area(i) = triangle_area ( t );

  end
%
%  Normalize the areas.
%
  area(1:nv) = area(1:nv) / sum ( area(1:nv) );
%
%  Replace each area by the sum of itself and all previous ones.
%
  for i = 2 : nv
    area(i) = area(i) + area(i-1);
  end

  for i = 1 : n
%
%  Choose a triangle at random, based on areas.
%
    t = rand ( 1, 1 );

    for k = 1 : nv

      if ( t <= area(k) )
        i2 = k;
        break
      end

    end
%
%  Now choose a point at random in the triangle.
%
    if ( i2 < nv )
      i2p1 = i2 + 1;
    else
      i2p1 = 1;
    end

    r = rand ( 2, 1 );

    if ( 1.0 < sum ( r ) )
      r = 1.0 - r;
    end

    x(i,:) = ( 1.0 - r(1) - r(2) ) * v(i2,:) ...
                          + r(1)   * v(i2p1,:) ...
                          + r(2)   * centroid(1,:);

  end

  return
end
