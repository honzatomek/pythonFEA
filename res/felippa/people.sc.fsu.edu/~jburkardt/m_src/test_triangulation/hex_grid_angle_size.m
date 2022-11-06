function n = hex_grid_angle_size ( box, center, angle, h )

%*****************************************************************************80
%
%% hex_grid_angle_size() counts the points in an angled hex grid in a box.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    10 April 2009
%
%  Author:
%
%    John Burkardt
%
%  Input:
%
%    real BOX(DIM_num,2), the lower left and upper right
%    corners of the box.
%
%    real CENTER(DIM_num), the center of the grid.
%    This point must be inside the box
%
%    real ANGLE, the angle, in degrees, of the grid.
%    Normally, 0 <= ANGLE <= 180, but any value is allowed.
%
%    real H, the spacing between neighboring
%    points on a grid line.
%
%  Output:
%
%    integer N, the number of points of the angled hex grid
%    that are within the unit square.
%
  dim_num = 2;
%
%  Ninny checks.
%
  if ( ~box_contains_point_2d ( box, center ) )
    fprintf ( 1, '\n' );
    fprintf ( 1, 'HEX_GRID_ANGLE_SIZE - Fatal error!\n' );
    fprintf ( 1, '  The center point of the grid is not\n' );
    fprintf ( 1, '  inside the box.\n' );
    fprintf ( 1, '  CENTER = (%f,%f)\n', center(1:dim_num) );
    error ( 'HEX_GRID_ANGLE_SIZE - Fatal error!' );
  end

  if ( h == 0.0 )
    fprintf ( 1, '\n' );
    fprintf ( 1, 'HEX_GRID_ANGLE_SIZE - Fatal error!\n' );
    fprintf ( 1, '  The grid spacing must be nonzero.\n' );
    fprintf ( 1, '  H = %f\n', h );
    error ( 'HEX_GRID_ANGLE_SIZE - Fatal error!' );
  end

  n = 0;

  layer = 0;
  point(1:dim_num) = center(1:dim_num);

  n = 1;

  while ( true )

    layer = layer + 1;

    layer_size = 0;

    angle2 = angle;
%
%  Compute the first point on the new layer.
%
    point(1) = point(1) + h * cos_deg ( angle2 );
    point(2) = point(2) + h * sin_deg ( angle2 );

    angle2 = r8_modp ( angle2 + 60.0, 360.0 );

    for i = 1 : 6

      angle2 = r8_modp ( angle2 + 60.0, 360.0 );

      for j = 1 : layer

        point(1) = point(1) + h * cos_deg ( angle2 );
        point(2) = point(2) + h * sin_deg ( angle2 );

        if ( box_contains_point_2d ( box, point ) )
          layer_size = layer_size + 1;
          n = n + 1;
        end

      end
    end

    if ( layer_size == 0 )
      break
    end

  end

  return
end
