function area = sphere_imp_area_nd ( dim_num, r )

%*****************************************************************************80
%
%% sphere_imp_area_nd() computes the surface area of an implicit sphere in ND.
%
%  Discussion:
%
%    An implicit sphere in ND satisfies the equation:
%
%      sum ( ( P(1:N) - CENTER(1:N) )^2 ) = R^2
%
%    DIM_NUM   Area
%
%    2      2       * PI   * R
%    3      4       * PI   * R^2
%    4      2       * PI^2 * R^3
%    5      (8/3)   * PI^2 * R^4
%    6                PI^3 * R^5
%    7      (16/15) * PI^3 * R^6
%
%    Sphere_Area ( DIM_NUM, R ) = 2 * PI^(DIM_NUM/2) * R^(DIM_NUM-1) / Gamma ( DIM_NUM / 2 )
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    05 February 2005
%
%  Author:
%
%    John Burkardt
%
%  Input:
%
%    integer DIM_NUM, the dimension of the space.
%
%    real R, the radius of the sphere.
%
%  Output:
%
%    real AREA, the area of the sphere.
%
  area = r^( dim_num -1  ) * sphere01_area_nd ( dim_num );

  return
end
