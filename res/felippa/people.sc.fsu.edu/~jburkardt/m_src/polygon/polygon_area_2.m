function area = polygon_area_2 ( n, v )

%*****************************************************************************80
%
%% polygon_area_2() computes the area of a polygon.
%
%  Discussion:
%
%    The area is the sum of the areas of the triangles formed by
%    node N with consecutive pairs of nodes.
%
%    If the vertices are given in counterclockwise order, the area
%    will be positive.  If the vertices are given in clockwise order,
%    the area will be negative.
%
%    Thanks to Martin Pineault for noticing that an earlier version
%    of this routine would not correctly compute the area of a nonconvex
%    polygon.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    17 October 2005
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
%    real AREA, the area of the polygon.
%
  area = 0.0;

  for i = 1 : n - 2

    t(1:2,1:3) = [ v(1:2,i)'; v(1:2,i+1)'; v(1:2,n)' ]';

    area_triangle = triangle_area ( t );

    area = area + area_triangle;

  end

  return
end
