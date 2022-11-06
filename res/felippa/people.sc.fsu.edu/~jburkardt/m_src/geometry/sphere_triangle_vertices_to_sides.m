function [ as, bs, cs ] = sphere_triangle_vertices_to_sides ( r, v1, v2, v3 )

%*****************************************************************************80
%
%% sphere_triangle_vertices_to_sides() computes spherical triangle sides.
%
%  Discussion:
%
%    We can use the ACOS system call here, but the ARC_COSINE routine
%    will automatically take care of cases where the input argument is
%    (usually slightly) out of bounds.
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
%    real R, the radius of the sphere.
%
%    real V1(3), V2(3), V3(3), the vertices of the spherical
%    triangle.
%
%  Output:
%
%    real AS, BS, CS, the (geodesic) length of the sides
%    of the triangle.
%
  dim_num = 3;

  as = r * acos ( v2(1:dim_num)' * v3(1:dim_num) / r.^2 );
  bs = r * acos ( v3(1:dim_num)' * v1(1:dim_num) / r.^2 );
  cs = r * acos ( v1(1:dim_num)' * v2(1:dim_num) / r.^2 );

  return
end
