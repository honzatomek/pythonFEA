function [ pn, dist ] = circle_imp_point_near_2d ( r, center, p )

%*****************************************************************************80
%
%% circle_imp_point_near_2d(): nearest ( implicit circle, point ) in 2D.
%
%  Discussion:
%
%    This routine finds the distance from a point to an implicitly
%    defined circle, and returns the point on the circle that is
%    nearest to the given point.
%
%    If the given point is the center of the circle, than any point
%    on the circle is "the" nearest.
%
%    An implicit circle in 2D satisfies the equation:
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
%    real PN(2), the nearest point on the circle.
%
%    real DIST, the distance of the point to the circle.
%
  ndim = 2;

  if ( p(1:ndim) == center(1:ndim) )
    dist = r;
    pn(1:ndim) = center(1:ndim) + r / sqrt ( ndim );
    return
  end

  r2 = sqrt ( sum ( ( p(1:ndim) - center(1:ndim) ).^2 ) );

  dist = abs (  r2 - r );

  pn(1:ndim) = center(1:ndim) + r * ( p(1:ndim) - center(1:ndim) ) / r2;

  return
end
