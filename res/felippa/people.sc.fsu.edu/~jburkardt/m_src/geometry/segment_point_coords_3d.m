function [ s, t ] = segment_point_coords_2d ( p1, p2, p )

%*****************************************************************************80
%
%% segment_point_coords_3d(): coordinates of a point on a line segment in 3D.
%
%  Discussion:
%
%    A line segment is the finite portion of a line that lies between
%    two points P1 and P2.
%
%    By the coordinates of a point P with respect to a line segment [P1,P2]
%    we mean numbers S and T such that S gives us the distance from the
%    point P to the nearest point PN on the line (not the line segment!),
%    and T gives us the position of PN relative to P1 and P2.
%
%    If S is zero, then P lies on the line.
%
%    If 0 <= T <= 1, then PN lies on the line segment.
%
%    If both conditions hold, then P lies on the line segment.
%
%    If E is the length of the line segment, then the distance of the
%    point to the line segment is:
%
%      sqrt ( S**2 +  T**2    * E**2 )     if      T <= 0;
%             S                            if 0 <= T <= 1
%      sqrt ( S**2 + (T-1)**2 * E**2 )     if 1 <= T
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    05 August 2006
%
%  Author:
%
%    John Burkardt
%
%  Input:
%
%    real P1(3), P2(3), the endpoints of the line segment.
%
%    real P(3), the point to be considered.
%
%%  Output:
%
%    real S, the distance of P to the nearest point PN
%    on the line through P1 and P2.  (S will always be nonnegative.)
%
%    real T, the relative position of the point PN
%    to the points P1 and P2.
%
  dim_num = 3;
%
%  If the line segment is actually a point, then the answer is easy.
%
  if ( p1(1:dim_num) == p2(1:dim_num) )

    t = 0.0;

  else

    bot = sum ( ( p2(1:dim_num) - p1(1:dim_num) ).^2 );

    t = ( p(1:dim_num)  - p1(1:dim_num) ) ...
      * ( p2(1:dim_num) - p1(1:dim_num) )' / bot;

  end

  pn(1:dim_num) = p1(1:dim_num) + t * ( p2(1:dim_num) - p1(1:dim_num) );

  s = sqrt ( sum ( ( p(1:dim_num) - pn(1:dim_num) ).^2 ) );

  return
end
