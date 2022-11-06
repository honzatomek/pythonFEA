function inside = angle_contains_point_2d ( p1, p2, p3, p )

%*****************************************************************************80
%
%% angle_contains_point_2d() determines if an angle contains a point, in 2D.
%
%  Discussion:
%
%    The angle is defined by the sequence of points P1, P2 and P3.
%
%    The point is "contained" by the angle if the ray P - P2
%    is between (in a counterclockwise sense) the rays P1 - P2
%    and P3 - P2.
%
%        P1
%        /
%       /   P
%      /  .
%     / .
%    P2--------->P3
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    18 September 2020
%
%  Author:
%
%    John Burkardt
%
%  Input:
%
%    real P1(2,1), P2(2,1), P3(2,1), the coordinates of
%    three points that define the angle.  The order of these points matters!
%
%    real P(2,1), the point to be checked.
%
%  Output:
%
%    logical INSIDE, is TRUE if the point is inside the angle.
%
  if ( angle_rad ( p1, p2, p ) <= angle_rad ( p1, p2, p3 ) )
    inside = true;
  else
    inside = false;
  end

  return
end
