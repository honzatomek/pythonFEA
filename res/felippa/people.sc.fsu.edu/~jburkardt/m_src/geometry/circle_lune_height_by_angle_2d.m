function height = circle_lune_height_by_angle_2d ( r, angle )

%*****************************************************************************80
%
%% circle_lune_height_by_angle_2d() computes the height of a circular lune.
%
%  Discussion:
%
%    Draw the chord connecting two points on the circumference of a circle.
%    The region between the chord and the circumference is a "lune".
%    The lune subtends a given angle between 0 and 2 pi.
%
%    The distance from the center of the circle to the midpoint of the chord
%    is the "height" H of the lune and we wish to determine this value.
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
%    real R, the radius of the circle.
%
%    real ANGLE, the angle subtended by the lune.
%
%  Output:
%
%    real HEIGHT, the height of the lune.
%
  height = r * cos ( angle / 2.0 );

  return
end
