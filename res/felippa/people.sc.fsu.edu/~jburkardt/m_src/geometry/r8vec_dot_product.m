function value = r8vec_dot_product ( n, v1, v2 )

%*****************************************************************************80
%
%% r8vec_dot_product() finds the dot product of a pair of R8VEC's.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    04 February 2016
%
%  Author:
%
%    John Burkardt
%
%  Input:
%
%    real V1(N), V2(N), the vectors.
%
%  Output:
%
%    real VALUE, the dot product.
%

%
%  Destroy all row vectors!
%
  v1 = v1(:);
  v2 = v2(:);

  value = v1' * v2;

  return
end
