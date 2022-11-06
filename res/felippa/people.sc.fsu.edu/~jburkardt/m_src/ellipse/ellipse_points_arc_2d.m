function p = ellipse_points_arc_2d ( center, r1, r2, psi, theta1, theta2, n )

%*****************************************************************************80
%
%% ellipse_points_arc_2d() returns N points on a tilted elliptical arc in 2D.
%
%  Discussion:
%
%    The points are "equally spaced" in the angular sense.  They are
%    not equally spaced along the perimeter of the ellipse.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    08 May 2005
%
%  Author:
%
%    John Burkardt
%
%  Input:
%
%    real CENTER(2), the coordinates of the center of the ellipse.
%
%    real R1, R2, the "radius" of the ellipse in the major
%    and minor axis directions.  A circle has these values equal.
%
%    real PSI, the angle that the major axis of the ellipse
%    makes with the X axis.  A value of 0.0 means that the major and
%    minor axes of the ellipse will be the X and Y coordinate axes.
%
%    real THETA1, THETA2, the angular coordinates of
%    the first and last points to be drawn, in radians.  This angle is measured
%    with respect to the (possibly tilted) major axis.
%
%    integer N, the number of points desired.  N must be at least 1.
%
%  Output:
%
%    real P(2,N), points on the ellipse.
%
  dim_num = 2;
%
%  THETA3 is the smallest angle, no less than THETA1, which
%  coincides with THETA2.
%
  theta3 = theta1 + r8_modp ( theta2 - theta1, 2.0 * pi );

  for i = 1 : n

    if ( 1 < n )
      theta = ( ( n - i     ) * theta1 ...
              + (     i - 1 ) * theta3 ) ...
              / ( n     - 1 );
    else
      theta = 0.5 * ( theta1 + theta3 );
    end

    p(1,i) = center(1) + r1 * cos ( psi ) * cos ( theta ) ...
                       - r2 * sin ( psi ) * sin ( theta );

    p(2,i) = center(2) + r1 * sin ( psi ) * cos ( theta ) ...
                       + r2 * cos ( psi ) * sin ( theta );

  end

  return
end
