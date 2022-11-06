function inside = circle_imp_contains_point_2d ( r, center, p )

%*****************************************************************************80
%
%% circle_imp_contains_point_2d(): implicit circle contains a point in 2D?
%
%  Discussion:
%
%    An implicit circle in 2D satisfies:
%
%      ( X - CENTER(1) )^2 + ( Y - CENTER(2) )^2 = R^2
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
%    real P(2), the point to be checked.
%
%  Output:
%
%    logical INSIDE, is TRUE if the point is inside or on the circle,
%    FALSE otherwise.
%
  if ( ( p(1) - center(1) ) * ( p(1) - center(1) ) ...
     + ( p(2) - center(2) ) * ( p(2) - center(2) ) <= r * r )
    inside = 1;
  else
    inside = 0;
  end

  return
end
