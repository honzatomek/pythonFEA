function inside = triangle_contains_point_1 ( t, p )

%*****************************************************************************80
%
%% triangle_contains_point_1() finds if a point is inside a triangle.
%
%  Discussion:
%
%    It is conventional to list the triangle vertices in counter clockwise
%    order.  However, this routine does not require a particular order
%    for the vertices.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    05 December 2010
%
%  Author:
%
%    John Burkardt
%
%  Input:
%
%    real T(2,3), the triangle vertices.
%
%    real P(2,1), the point to be checked.
%
%  Output:
%
%    logical INSIDE, is TRUE if the point is inside
%    the triangle or on its boundary.
%
  xsi = triangle_barycentric ( t, p );

  if ( any ( xsi(1:3,1) < 0.0 ) )
    inside = false;
  else
    inside = true;
  end

  return
end
