function [ pp, normal ] = plane_normal_uniform_3d ( )

%*****************************************************************************80
%
%% plane_normal_uniform_3d() generates a random normal plane in 3D.
%
%  Discussion:
%
%    The normal form of a plane is:
%
%      PP is a point on the plane,
%      N is a normal vector to the plane.
%
%    The point PP will be chosen at random inside the unit sphere.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    26 November 2005
%
%  Author:
%
%    John Burkardt
%
%  Output:
%
%    real PP(3), a point on the plane.
%
%    real NORMAL(3), the unit normal vector.
%
  dim_num = 3;
%
%  Pick PP as a random point inside the unit sphere in ND.
%
  pp = ball_unit_sample_3d ( );
%
%  Get values from a standard normal distribution.
%
  normal = randn ( dim_num, 1 );
%
%  Compute the length of the vector.
%
  n_norm = norm ( normal );
%
%  Normalize the vector.
%
  normal = normal / n_norm;

  return
end
