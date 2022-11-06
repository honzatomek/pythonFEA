function value = hexagon_area_2d ( r )

%*****************************************************************************80
%
%% hexagon_area_2d() returns the area of a regular hexagon in 2D.
%
%  Discussion:
%
%    The formula for the area only requires the radius, and does
%    not depend on the location of the center, or the orientation
%    of the hexagon.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license. 
%
%  Modified:
%
%    26 May 2004
%
%  Author:
%
%    John Burkardt
%
%  Input:
%
%    real R, the radius of the hexagon.
%
%  Output:
%
%    real VALUE, the area of the hexagon.
%
  value = r * r * hexagon_unit_area_2d ( );

  return
end
