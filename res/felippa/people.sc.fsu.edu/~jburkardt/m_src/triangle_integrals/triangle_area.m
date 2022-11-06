function area = triangle_area ( t )

%*****************************************************************************80
%
%% triangle_area() returns the area of a triangle.
%
%  Discussion:
%
%    If the vertices are given in counter clockwise order, the area
%    will be positive.
%
%  Licensing:
%
%    This code is distributed under the GNU GPL license.
%
%  Modified:
%
%    18 April 2015
%
%  Author:
%
%    John Burkardt.
%
%  Input:
%
%    real T(2,3), the vertices of the triangle.
%
%  Output:
%
%    real AREA, the area of the triangle.
%
  area = 0.5 * ...
    ( ...
        ( t(1,2) - t(1,1) ) * ( t(2,3) - t(2,1) ) ...
      - ( t(1,3) - t(1,1) ) * ( t(2,2) - t(2,1) ) ...
    );

  return
end
