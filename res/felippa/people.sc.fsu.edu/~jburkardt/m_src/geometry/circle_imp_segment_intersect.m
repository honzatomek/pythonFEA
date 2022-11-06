function [ int_num, p ] = circle_imp_segment_intersect ( r, pc, p0, p1 )

%*****************************************************************************80
%
%% circle_imp_segment_intersect(): ( imp circle, line segment ) intersection.
%
%  Discussion:
%
%    Points P on an implicit circle in 2D satisfy the equation:
%
%      ( P(1) - PC(1) )^2 + ( P(2) - PC(2) )^2 = R^2
%
%    A line segment in 2D is:
%
%      P(t) = (1-t) * P0 + t * P1
%
%    where P0 and P1 are distinct points and 0 <= t <= 1.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license. 
%
%  Modified:
%
%    14 September 2020
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
%    real P0(2), P1(2), the endpoints of the line segment.
%
%  Output:
%
%    integer  INT_NUM, the number of intersections.
%    INT_NUM will be 0, 1 or 2.
%
%    real P(2,INT_NUM), the intersecting points.
%

%
%  Find intersections of the full line with the circle.
%
  [ int_num_line, p_line ] = circle_imp_line_exp_intersect ( r, pc, p0, p1 );
%
%  Only accept intersection points that lie between P0 and P1.
%
  int_num = 0;
  p = zeros ( 2, 0 );

  for j = 1 : int_num_line

    p2(1:2,1) = p_line(1:2,j);

    [ s, t ] = segment_point_coords_2d ( p0, p1, p2 );

    if ( 0.0 <= t & t <= 1.0 )
      int_num = int_num + 1;
      p(1:2,int_num) = p2(1:2,1);
    end

  end
  
  return
end
