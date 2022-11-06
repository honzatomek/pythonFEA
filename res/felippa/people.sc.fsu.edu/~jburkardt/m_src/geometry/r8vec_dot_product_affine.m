function value = r8vec_dot_product_affine ( n, v0, v1, v2 )

%*****************************************************************************80
%
%% r8vec_dot_product_affine() computes the affine dot product V1-V0 * V2-V0.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license. 
%
%  Modified:
%
%    27 October 2010
%
%  Author:
%
%    John Burkardt
%
%  Input:
%
%    integer N, the spatial dimension.
%
%    real V0(N), the base vector.
%
%    real V1(N), V2(N), the vectors.
%
%  Output:
%
%    real VALUE, the dot product.
%
  value = ( v1(1:n) - v0(1:n) )' * ( v2(1:n) - v0(1:n) );

  return
end
