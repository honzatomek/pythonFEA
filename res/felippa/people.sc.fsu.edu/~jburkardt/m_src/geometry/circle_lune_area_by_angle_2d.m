function value = circle_lune_area_by_angle_2d ( r, center, theta1, theta2 )

%*****************************************************************************80
%
%% circle_lune_area_by_angle_2d() returns the area of a circular lune in 2D.
%
%  Discussion:
%
%    A lune is formed by drawing a circular arc, and joining its endpoints.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    05 May 2004
%
%  Author:
%
%    John Burkardt
%
%  Input:
%
%    real R, the radius of the circle.
%
%    real CENTER(2,1), the center of the circle.
%
%    real THETA1, THETA2, the angles of the rays
%    that begin and end the arc.
%
%  Output:
%
%    real VALUE, the area of the lune.
%
  sector = circle_sector_area_2d ( r, center, theta1, theta2 );
  triangle = circle_triangle_area_2d ( r, center, theta1, theta2 );
  value = sector - triangle;

  return
end
