function area = polygon_area ( nv, v )

%*****************************************************************************80
%
%% polygon_area() determines the area of a polygon.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    07 May 2014
%
%  Author:
%
%    John Burkardt
%
%  Input:
%
%    integer NV, the number of vertices of the polygon.
%
%    real V(2,NV), the vertex coordinates.
%
%  Output:
%
%    real AREA, the area of the polygon.
%
  e(1) = 0;
  e(2) = 0;

  area = polygon_monomial_integral ( nv, v, e );

  return
end
