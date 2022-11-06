function dist = circle_imp_line_exp_dist_2d ( r, pc, p1, p2 )

%*****************************************************************************80
%
%% circle_imp_line_exp_dist_2d(): distance ( implicit circle, explicit line ) in 2D.
%
%  Discussion:
%
%    The distance is zero if the line intersects the circle.
%
%    Points P on an implicit circle in 2D satisfy the equation:
%
%      ( P(1) - PC(1) )^2 + ( P(2) - PC(2) )^2 = R^2
%
%    The explicit form of a line in 2D is:
%
%      the line through the points P1 and P2.
%
%    The distance between the circle and the line is zero if
%    and only if they intersect.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license. 
%
%  Modified:
%
%    15 September 2020
%
%  Author:
%
%    John Burkardt
%
%  Input:
%
%    real R, the radius of the circle.
%
%    real PC(2), the center of the circle.
%
%    real  P1(2), P2(2), two points on the line.
%
%  Output:
%
%    real DIST, the distance of the line to the circle.
%
  dist = line_exp_point_dist_2d ( p1, p2, pc );

  dist = dist - r;

  dist = max ( dist, 0.0 );

  return
end
