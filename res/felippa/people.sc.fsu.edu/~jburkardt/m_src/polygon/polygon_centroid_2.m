function centroid = polygon_centroid_2 ( n, v )

%*****************************************************************************80
%
%% polygon_centroid_2() computes the centroid of a polygon.
%
%  Discussion:
%
%    The centroid is the area-weighted sum of the centroids of
%    disjoint triangles that make up the polygon.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    09 November 2016
%
%  Author:
%
%    John Burkardt
%
%  Reference:
%
%    Adrian Bowyer and John Woodwark,
%    A Programmer's Geometry,
%    Butterworths, 1983.
%
%  Input:
%
%    integer N, the number of vertices of the polygon.
%
%    real V(2,N), the coordinates of the vertices.
%
%  Output:
%
%    real CENTROID(2,1), the coordinates of the centroid.
%
  centroid = zeros ( 2, 1 );

  area = 0.0;

  for i = 1 : n - 2

    t(1:2,1:3) = [ v(1:2,i)'; v(1:2,i+1)'; v(1:2,n)' ]';

    area_triangle = triangle_area ( t );

    area = area + area_triangle;

    centroid(1:2,1) = centroid(1:2,1) + area_triangle ...
      * ( v(1:2,i) + v(1:2,i+1) + v(1:2,n) ) / 3.0;

  end

  if ( area == 0.0 )
    centroid(1:2,1) = v(1:2,1);
  else
    centroid(1:2,1) = centroid(1:2,1) / area;
  end

  return
end
