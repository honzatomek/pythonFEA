function value = r8vec_cross_product_affine_2d ( v0, v1, v2 )

%*****************************************************************************80
%
%% r8vec_cross_product_affine_2d() finds the affine cross product in 2D.
%
%  Discussion:
%
%    Strictly speaking, the vectors V1 and V2 should be considered
%    to lie in a 3D space, both having Z coordinate zero.  The cross 
%    product value V3 then represents the standard cross product vector 
%    (0,0,V3).
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
%    real V0(2), the base vector.
%
%    real V1(2), V2(2), the vectors.
%
%  Output:
%
%    real VALUE, the cross product (V1-V0) x (V2-V0).
%
  value = ...
      ( v1(1) - v0(1) ) * ( v2(2) - v0(2) ) ...
    - ( v2(1) - v0(1) ) * ( v1(2) - v0(2) );

  return
end
