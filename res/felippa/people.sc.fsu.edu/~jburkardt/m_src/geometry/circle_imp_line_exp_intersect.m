function [ int_num, p ] = circle_imp_line_exp_intersect ( r, pc, p0, p1  )

%*****************************************************************************80
%
%% circle_imp_line_exp_intersect(): ( imp circle, exp line ) intersection.
%
%  Discussion:
%
%    Points P on an implicit circle in 2D satisfy the equation:
%
%      ( P(1) - PC(1) )^2 + ( P(2) - PC(2) )^2 = R^2
%
%    An explicit line in 2D is:
%
%      P(t) = (1-t) * P0 + t * P1
%
%    where P0 and P1 are two distinct points on the line.
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
%  Reference:
%
%    Eric W Weisstein, 
%    "Circle-Line Intersection." 
%    From MathWorld--A Wolfram Web Resource. 
%    https://mathworld.wolfram.com/Circle-LineIntersection.html
%
%  Input:
%
%    real R, the radius of the circle.
%
%    real PC(2), the center of the circle.
%
%    real P0(2), P1(2), two points on the explicit line.
%
%  Output:
%
%    integer INT_NUM, the number of intersections.
%    INT_NUM will be 0, 1 or 2.
%
%    real P(2,INT_NUM), the intersecting points.
%

%
%  Solve the problem in the coordinate system where the circle 
%  has center (0,0).
%
  x1 = p0(1) - pc(1);
  y1 = p0(2) - pc(2);

  x2 = p1(1) - pc(1);
  y2 = p1(2) - pc(2);

  dx = x2 - x1;
  dy = y2 - y1;

  dr = sqrt ( dx * dx + dy * dy );
  d = x1 * y2 - x2 * y1;

  disc = r^2 * dr^2 - d^2;

  if ( disc < 0.0 )
    int_num = 0;
    p = zeros ( 2, 0 );
  elseif ( disc == 0.0 )
    int_num = 1;
    xp = (   d * dy ) / dr^2;
    yp = ( - d * dx ) / dr^2;
    p(1,1) = xp + pc(1);
    p(2,1) = yp + pc(2);
  else
    int_num = 2;
    xp = (   d * dy + sign ( dy ) * dx * sqrt ( disc ) ) / dr^2;
    xm = (   d * dy - sign ( dy ) * dx * sqrt ( disc ) ) / dr^2;
    yp = ( - d * dx + abs ( dy )       * sqrt ( disc ) ) / dr^2;
    ym = ( - d * dx - abs ( dy )       * sqrt ( disc ) ) / dr^2;
    p(1,1) = xp + pc(1);
    p(2,1) = yp + pc(2);
    p(1,2) = xm + pc(1);
    p(2,2) = ym + pc(2);
  end

  return
end
