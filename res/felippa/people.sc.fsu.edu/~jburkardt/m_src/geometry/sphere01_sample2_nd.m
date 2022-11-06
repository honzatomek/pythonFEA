function x = sphere01_sample2_nd ( dim_num )

%*****************************************************************************80
%
%% sphere01_sample2_nd() picks a random point on the unit sphere in ND.
%
%  Discussion:
%
%    The unit sphere in ND satisfies:
%
%      sum ( 1 <= I <= DIM_NUM ) X(I) * X(I) = 1
%
%    DIM_NUM independent normally distributed random numbers are generated,
%    and then scaled to have unit norm.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    19 February 2005
%
%  Author:
%
%    John Burkardt
%
%  Input:
%
%    integer DIM_NUM, the dimension of the space.
%
%  Output:
%
%    real X(DIM_NUM), the random point.
%
  x = randn ( dim_num, 1 );

  x_norm = norm ( x );

  x = x / x_norm;

  return
end
