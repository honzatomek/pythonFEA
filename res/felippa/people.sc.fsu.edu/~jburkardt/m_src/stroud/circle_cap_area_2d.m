function area = circle_cap_area_2d ( r, h )

%*****************************************************************************80
%
%% circle_cap_area_2d() computes the area of a circle cap in 2D.
%
%  Discussion:
%
%    Draw any radius R of the circle and denote as P the point where the
%    radius intersects the circle.  Now consider the point Q which lies
%    on the radius and which is H units from P.  The line which is
%    perpendicular to the radius R and passes through Q divides the
%    circle into two pieces.  The piece including the point P is the
%    circular cap of height (or thickness) H.
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
%  Input:
%
%    real R, the radius of the circle.
%
%    real H, the "height" of the circle cap.  
%
%  Output:
%
%    real AREA, the area of the circle cap.
%
  if ( h <= 0.0 )

    area = 0.0;

  elseif ( h <= r )

    theta = 2.0 * asin ( sqrt ( h * ( 2.0 * r - h ) ) / r );
    area = r * r * ( theta - sin ( theta ) ) / 2.0;

  elseif ( h <= 2.0 * r )

    theta = 2.0 * asin ( sqrt ( h * ( 2.0 * r - h ) ) / r );
    area = r * r * ( pi - ( theta - sin ( theta ) ) / 2.0 );

  elseif ( 2.0 * r <= h )

    area = pi * r * r;

  end

  return
end
