function dist = ellipse_point_dist_2d ( a, b, p )

%*****************************************************************************80
%
%% ellipse_point_dist_2d() finds the distance from a point to an ellipse in 2D.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    08 March 2005
%
%  Author:
%
%    John Burkardt
%
%  Reference:
%
%    Dianne O'Leary,
%    Elastoplastic Torsion: Twist and Stress,
%    Computing in Science and Engineering,
%    July/August 2004, pages 74-76.
%    September/October 2004, pages 63-65.
%
%  Input:
%
%    real A, B, the ellipse parameters.  Normally,
%    these are both positive quantities.  Generally, they are also
%    distinct.
%
%    real P(2,1), the point.
%
%  Output:
%
%    real DIST, the distance to the ellipse.
%
  p = p(:);

  pn = ellipse_point_near_2d ( a, b, p );

  dist = norm ( p - pn );

  return
end
