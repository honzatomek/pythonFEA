function value = r8vec_distance ( dim_num, v1, v2 )

%*****************************************************************************80
%
%% r8vec_distance() returns the Euclidean distance between two R8VEC's.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    11 August 2005
%
%  Author:
%
%    John Burkardt
%
%  Input:
%
%    integer DIM_NUM, the spatial dimension.
%
%    real V1(DIM_NUM), V2(DIM_NUM), the vectors.
%
%  Output:
%
%    real VALUE, the Euclidean distance
%    between the vectors.
%
  value = sqrt ( sum ( ( v1(1:dim_num) - v2(1:dim_num) ).^2 ) );

  return
end
