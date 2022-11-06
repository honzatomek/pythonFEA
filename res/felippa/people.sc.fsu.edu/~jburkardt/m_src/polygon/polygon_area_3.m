function area = polygon_area_3 ( n, v )

%*****************************************************************************80
%
%% polygon_area_3() computes the area of a polygon.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    05 April 2020
%
%  Author:
%
%    John Burkardt
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
  x = v(1,:)';
  x2 = [ x(2:n); x(1) ];
  y = v(2,:)';
  y2 = [ y(2:n); y(1) ];

  area = 0.5 * ( x' * y2 - x2' * y );

  return
end
