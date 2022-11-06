function area = circles_intersect_area_2d ( r1, r2, d )

%*****************************************************************************80
%
%% circles_intersect_area_2d(): area of the intersection of two circles.
%
%  Discussion:
%
%    Circles of radius R1 and R2 are D units apart.  What is the area of
%    intersection?
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    14 January 2018
%
%  Author:
%
%    John Burkardt
%
%  Input:
%
%    real R1, R2, the radiuses of the circles.
%    R1 and R2 should be positive.
%
%    real D, the distance between the circular centers.
%    D must be positive, and should be no greater than R1 + R2.
%
%  Output:
%
%    real AREA, the area of the intersection.
%
  if ( r1 + r2 < d )
    area = 0.0;
  elseif ( d == 0 )
    area = pi * ( min ( r1, r2 ) )^2;
  else
    h1 = 0.5 * ( d^2 + r1^2 - r2^2 ) / d;
    area1 = circle_lune_area_by_height_2d ( r1, h1 );
    h2 = 0.5 * ( d^2 - r1^2 + r2^2 ) / d;
    area2 = circle_lune_area_by_height_2d ( r2, h2 );
    area = area1 + area2;
  end

  return
end
