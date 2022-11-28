function [ pn, dist, t ] = segment_point_near ( p1, p2, p )

%*****************************************************************************80
%
%% segment_point_near() finds the line segment point nearest a point.
%
%  Discussion:
%
%    A line segment is the finite portion of a line that lies between
%    two points.
%
%    The nearest point will satisfy the condition
%
%      PN = (1-T) * P1 + T * P2.
%
%    T will always be between 0 and 1.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    04 December 2010
%
%  Author:
%
%    John Burkardt
%
%  Input:
%
%    real P1(2), P2(2), the endpoints of the line segment.
%
%    real P(2), the point whose nearest neighbor
%    on the line segment is to be determined.
%
%  Output:
%
%    real PN(2), the point on the line segment which is
%    nearest the point (X,Y).
%
%    real DIST, the distance from the point to the
%    nearest point on the line segment.
%
%    real T, the relative position of the point (XN,YN)
%    to the points (X1,Y1) and (X2,Y2).
%
  p  = p(:);
  p1 = p1(:);
  p2 = p2(:);
%
%  If the line segment is actually a point, then the answer is easy.
%
  if ( p1(1:2,1) == p2(1:2,1) )

    t = 0.0;

  else

    bot = sum ( ( p2(1:2,1) - p1(1:2,1) ).^2 );

    t = ( p(1:2,1) - p1(1:2,1) )' * ( p2(1:2,1) - p1(1:2,1) ) / bot;

    t = max ( t, 0.0 );
    t = min ( t, 1.0 );

  end

  pn(1:2,1) = p1(1:2,1) + t * ( p2(1:2,1) - p1(1:2,1) );

  dist = sqrt ( sum ( ( pn(1:2,1) - p(1:2,1) ).^2 ) );

  return
end