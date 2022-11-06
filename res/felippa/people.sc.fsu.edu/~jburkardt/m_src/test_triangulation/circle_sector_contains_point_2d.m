function inside = circle_sector_contains_point_2d ( r, center, theta1, theta2, p )

%*****************************************************************************80
%
%% circle_sector_contains_point_2d() : is a point inside a circular sector?
%
%  Discussion:
%
%    A circular sector is formed by a circular arc, and the two straight line 
%    segments that join its ends to the center of the circle.
%
%    A circular sector is defined by
%
%      ( X - CENTER(1) )^2 + ( Y - CENTER(2) )^2 = R^2
%
%    and
%
%      Theta1 <= Theta <= Theta2
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
%    real R, the radius of the circle.
%
%    real CENTER(2), the center of the circle.
%
%    real THETA1, THETA2, the angles defining the arc,
%    in radians.  Normally, THETA1 < THETA2.
%
%    real P(2), the point to be checked.
%
%  Output:
%
%    logical INSIDE, is TRUE if the point is inside or on the
%    circular sector, FALSE otherwise.
%
  inside = 0;
%
%  Is the point inside the (full) circle?
%
  if ( ( p(1) - center(1) ) * ( p(1) - center(1) ) ...
     + ( p(2) - center(2) ) * ( p(2) - center(2) ) <= r * r )
%
%  Is the point's angle within the arc's range?
%  Try to force the angles to lie between 0 and 2 * PI.
%
    theta = atan4 ( p(2) - center(2), p(1) - center(1) );

    if ( r8_modp ( theta  - theta1,  2.0 * pi ) <= ...
         r8_modp ( theta2 - theta1,  2.0 * pi ) )

      inside = 1;

    end

  end

  return
end
