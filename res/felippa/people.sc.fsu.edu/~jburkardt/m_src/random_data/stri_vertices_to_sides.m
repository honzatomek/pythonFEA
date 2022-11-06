function [ as, bs, cs ] = stri_vertices_to_sides ( r, v1, v2, v3 )

%*****************************************************************************80
%
%% stri_vertices_to_sides() computes spherical triangle sides.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    13 April 2022
%
%  Author:
%
%    John Burkardt
%
%  Input:
%
%    real R, the radius of the sphere.
%
%    real V1(1,3), V2(1,3), V3(1,3), the vertices of the spherical
%    triangle.
%
%  Output:
%
%    real AS, BS, CS, the (geodesic) length of the sides
%    of the triangle.
%
  as = r * acos ( v2 * v3' / r.^2 );
  bs = r * acos ( v3 * v1' / r.^2 );
  cs = r * acos ( v1 * v2' / r.^2 );

  return
end
