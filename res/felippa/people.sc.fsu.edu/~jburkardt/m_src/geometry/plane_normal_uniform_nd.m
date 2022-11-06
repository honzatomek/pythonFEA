function [ pp, normal ] = plane_normal_uniform_nd ( dim_num )

%*****************************************************************************80
%
%% plane_normal_uniform_nd() generates a random normal plane in ND.
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
%  Input:
%
%    integer DIM_NUM, the spatial dimension.
%
%  Output:
%
%    real PP(DIM_NUM), a point on the plane.
%
%    real NORMAL(DIM_NUM), the unit normal vector.
%

%
%  Pick PP as a random point inside the unit sphere in ND.
%
  pp = ball_unit_sample_nd ( dim_num );
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
