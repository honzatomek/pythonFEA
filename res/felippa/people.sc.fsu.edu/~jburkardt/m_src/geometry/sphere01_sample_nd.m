function x = sphere01_sample_nd ( dim_num )

%*****************************************************************************80
%
%% sphere01_sample_nd() picks a random point on the unit sphere in ND.
%
%  Discussion:
%
%    The unit sphere in ND satisfies:
%
%      sum ( 1 <= I <= DIM_NUM ) X(I) * X(I) = 1
%
%    DIM_NUM-1 random Givens rotations are applied to the point ( 1, 0, 0, ..., 0 ).
%
%    The I-th Givens rotation is in the plane of coordinate axes I and I+1,
%    and has the form:
%
%     [ cos ( theta )  - sin ( theta ) ] * x(i)      = x'(i)
%     [ sin ( theta )    cos ( theta ) ]   x(i+1)      x'(i+1)
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    07 February 2005
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
  x(1) = 1.0;
  x(2:dim_num) = 0.0;

  for i = 1 : dim_num-1
    random_cosine = - 1.0 + 2.0 * rand ( 1, 1 );
    random_sign = - 1 + 2 * floor ( 2.0 * rand ( 1, 1 ) );
    random_sine = random_sign * sqrt ( 1.0 - random_cosine^2 );
    xi = x(i);
    x(i  ) = random_cosine * xi;
    x(i+1) = random_sine   * xi;
  end

  return
end
