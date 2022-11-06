function dist = plane_imp_point_dist_3d ( a, b, c, d, p )

%*****************************************************************************80
%
%% plane_imp_point_dist_3d(): distance ( implicit plane, point ) in 3D.
%
%  Discussion:
%
%    The implicit form of a plane in 3D is:
%
%      A * X + B * Y + C * Z + D = 0
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    12 May 2005
%
%  Author:
%
%    John Burkardt
%
%  Reference:
%
%    Adrian Bowyer and John Woodwark,
%    A Programmer's Geometry,
%    Butterworths, 1983.
%
%  Input:
%
%    real A, B, C, D, the implicit plane parameters.
%
%    real P(3), the coordinates of the point.
%
%  Output:
%
%    real DIST, the distance from the point to the plane.
%
  dim_num = 3;

  norm = sqrt ( a * a + b * b + c * c );

  if ( norm == 0.0 )
    fprintf ( 1, '\n' );
    fprintf ( 1, 'PLANE_IMP_POINT_DIST_3D - Fatal error!\n' );
    fprintf ( 1, '  The plane normal vector is null.\n' );
    error ( 'PLANE_IMP_POINT_DIST_3D - Fatal error!' );
  end

  dist = abs ( a * p(1) + b * p(2) + c * p(3) + d ) / norm;

  return
end
