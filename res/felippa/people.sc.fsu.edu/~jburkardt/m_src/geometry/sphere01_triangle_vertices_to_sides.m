function [ as, bs, cs ] = sphere01_triangle_vertices_to_sides ( v1, v2, v3 )

%*****************************************************************************80
%
%% sphere01_triangle_vertices_to_sides(): spherical triangle sides on unit sphere.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    12 January 2021
%
%  Author:
%
%    John Burkardt
%
%  Input:
%
%    real V1(3), V2(3), V3(3), the vertices of the spherical
%    triangle.
%
%  Output:
%
%    real AS, BS, CS, the (geodesic) length of the sides
%    of the triangle.
%

%
%  Destroy all row vectors!
%
  v1 = v1(:);
  v2 = v2(:);
  v3 = v3(:);

  as = acos ( v2(1:3)' * v3(1:3) );
  bs = acos ( v3(1:3)' * v1(1:3) );
  cs = acos ( v1(1:3)' * v2(1:3) );

  return
end
