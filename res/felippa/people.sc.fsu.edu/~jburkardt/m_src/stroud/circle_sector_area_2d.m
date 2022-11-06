function value = circle_sector_area_2d ( r, theta1, theta2 )

%*****************************************************************************80
%
%% circle_sector_area_2d() returns the area of a circular sector in 2D.
%
%  Discussion:
%
%    A sector is contained within a circular arc and the lines joining each
%    endpoint of the arc to the center of the circle.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license. 
%
%  Modified:
%
%    20 May 2004
%
%  Author:
%
%    John Burkardt
%
%  Input:
%
%    real R, the radius of the circle.
%
%    real THETA1, THETA2, the angles of the rays
%    that delimit the sector.
%
%  Output:
%
%    real VALUE, the area of the sector.
%
  value = 0.50E+00 * r * r * ( theta2 - theta1 );

  return
end
