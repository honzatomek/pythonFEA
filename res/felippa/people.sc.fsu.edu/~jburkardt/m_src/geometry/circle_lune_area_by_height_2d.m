function area = circle_lune_area_by_height_2d ( r, h )

%*****************************************************************************80
%
%% circle_lune_area_by_height_2d() computes the area of a circular lune.
%
%  Discussion:
%
%    Draw the chord connecting two points on the circumference of a circle.
%    The region between the chord and the circumference is a "lune".
%    We wish to know the area of this region.
%
%    The distance from the center of the circle to the midpoint of the chord
%    is the "height" H of the lune.  It is natural to expect 0 <= H <= R.
%    However, if we allow -R <= H < 0 as well, this allows us to include
%    lunes which involve more than half the circle's area.
%
%    If H < -R or R < H, then no lune is formed and we have zero area.
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
%    real H, the height of the lune.
%
%  Output:
%
%    real AREA, the area of the lune.
%
  if ( -r <= h & h <= r )
    area = r^2 * acos ( h / r ) - h * sqrt ( r^2 - h^2 );
  else
    area = 0.0;
  end

  return
end
