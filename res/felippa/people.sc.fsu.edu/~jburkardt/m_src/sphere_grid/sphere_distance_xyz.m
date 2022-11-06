function dist = sphere_distance_xyz ( xyz1, xyz2, r )

%*****************************************************************************80
%
%% sphere_distance_xyz() computes great circle distances on a sphere.
%
%  Discussion:
%
%    XYZ coordinates are used.
%
%    We assume the points XYZ1 and XYZ2 lie on the same sphere.
%
%    This computation is a special form of the Vincenty formula.
%    It should be less sensitive to errors associated with very small
%    or very large angular separations.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    12 January 2021
%
%  Author:
%
%    John Burkardt
%
%  Reference:
%
%    "Great-circle distance",
%    Wikipedia.
%
%  Input:
%
%    real XYZ1(3,1), the coordinates of the first point.
%
%    real XYZ2(3,1), the coordinates of the second point.
%
%  Output:
%
%    real DIST, the great circle distance between the points.
%
  r = norm ( xyz1 );

  lat1 = asin ( xyz1(3,1) );
  lon1 = atan4 ( xyz1(2,1), xyz1(1,1) );

  lat2 = asin ( xyz2(3,1) );
  lon2 = atan4 ( xyz2(2,1), xyz2(1,1) );

  top = ( cos ( lat2 ) * sin ( lon1 - lon2 ) ).^2 ...
      + ( cos ( lat1 ) * sin ( lat2 ) ...
      -   sin ( lat1 ) * cos ( lat2 ) * cos ( lon1 - lon2 ) ).^2;

  top = sqrt ( top );

  bot = sin ( lat1 ) * sin ( lat2 ) ...
      + cos ( lat1 ) * cos ( lat2 ) * cos ( lon1 - lon2 );

  dist = r * atan2 ( top, bot );

  return
end
