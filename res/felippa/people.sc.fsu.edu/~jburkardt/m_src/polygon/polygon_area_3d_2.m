function area = polygon_area_3d_2 ( n, v, area )

%*****************************************************************************80
%
%% polygon_area_3d_2() computes the area of a polygon in 3D.
%
%  Discussion:
%
%    The computation is not valid unless the vertices of the polygon
%    lie in a plane, so that the polygon that is defined is "flat".
%
%    The polygon does not have to be "regular", that is, neither its
%    sides nor its angles need to be equal.
%
%    The area is computed as the sum of the areas of the triangles 
%    formed by the last node with consecutive pairs of nodes (1,2),
%    (2,3), ..., and (N-2,N-1).
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    18 October 2005
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
%    real V(3,N), the coordinates of the vertices.
%
%  Output:
%
%    real AREA, the area of the polygon.
%
  area_vector = zeros ( 3, 1 );

  for i = 1 : n - 2

    t(1:3,1:3) = [ v(1:3,i)'; v(1:3,i+1)'; v(1:3,n)' ]';

    area_vector_triangle = triangle_area_vector_3d ( t );

    area_vector(1:3,1) = area_vector(1:3,1) + area_vector_triangle(1:3,1);

  end

  area = 0.5 * sqrt ( sum ( area_vector(1:3).^2 ) );

  return
end
