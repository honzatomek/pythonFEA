function colin = points_colin_2d ( p1, p2, p3 )

%*****************************************************************************80
%
%% points_colin_2d() estimates the colinearity of 3 points in 2D.
%
%  Discussion:
%
%    The estimate of colinearity that is returned is the ratio
%    of the area of the triangle spanned by the points to the area
%    of the equilateral triangle with the same perimeter.
%
%    This estimate is 1 if the points are maximally noncolinear, 0 if the
%    points are exactly colinear, and otherwise is closer to 1 or 0 depending
%    on whether the points are far or close to colinearity.
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
%  Input:
%
%    real P1(2), P2(2), P3(2), the points.
%
%  Output:
%
%    real COLIN, the colinearity estimate.
%
  dim_num = 2;

  t(1:dim_num,1:3) = [ p1(1:dim_num); p2(1:dim_num); p3(1:dim_num) ]';

  area_triangle = triangle_area ( t );

  if ( area_triangle == 0.0 )

    colin = 0.0;

  else

    perim = sqrt ( sum ( ( p2(1:dim_num) - p1(1:dim_num) ).^2 ) ) ...
          + sqrt ( sum ( ( p3(1:dim_num) - p2(1:dim_num) ).^2 ) ) ...
          + sqrt ( sum ( ( p1(1:dim_num) - p3(1:dim_num) ).^2 ) );

    side = perim / 3.0;

    area2 = 0.25 * sqrt ( 3.0 ) * side * side;

    colin = abs ( area_triangle ) / area2;

  end

  return
end
