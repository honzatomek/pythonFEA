function theta = vector_separation_nd ( dim_num, v1, v2, theta )

%*****************************************************************************80
%
%% vector_separation_nd() finds the angular separation between vectors in ND.
%
%  Discussion:
%
%    Any two vectors lie in a plane, and are separated by a plane angle.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%   12 January 2021
%
%  Author:
%
%    John Burkardt
%
%  Input:
%
%    integer DIM_NUM, the dimension of the vectors.
%
%    real V1(DIM_NUM), V2(DIM_NUM), the two vectors.
%
%  Output:
%
%    real THETA, the angle between the two vectors.
%
  v1_norm = sqrt ( sum ( v1(1:dim_num).^2 ) );

  v2_norm = sqrt ( sum ( v2(1:dim_num).^2 ) );

  cos_theta = ( v1(1:dim_num) * v2(1:dim_num)' ) / ( v1_norm * v2_norm );

  theta = acos ( cos_theta );

  return
end
