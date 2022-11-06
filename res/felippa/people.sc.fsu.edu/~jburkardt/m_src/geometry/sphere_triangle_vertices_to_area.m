function area = sphere_triangle_vertices_to_area ( r, v1, v2, v3 )

%*****************************************************************************80
%
%% sphere_triangle_vertices_to_area() computes the area of a spherical triangle in 3D.
%
%  Discussion:
%
%    A sphere centered at 0 in 3D satisfies the equation:
%
%      X * X + Y * Y + Z * Z = R * R
%
%    A spherical triangle is specified by three points on the surface
%    of the sphere.
%
%    The area formula is known as Girard's formula.
%
%    The area of a spherical triangle is:
%
%      AREA = ( A + B + C - PI ) * R * R
%
%    where A, B and C are the (surface) angles of the triangle.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    29 April 2010
%
%  Author:
%
%    John Burkardt
%
%  Input:
%
%    real R, the radius of the sphere.
%
%    real V1(3), V2(3), V3(3), the vertices of the triangle.
%
%  Output:
%
%    real AREA, the area of the spherical triangle.
%

%
%  Compute the lengths of the sides of the spherical triangle.
%
  [ as, bs, cs ] = sphere_triangle_vertices_to_sides ( r, v1, v2, v3 );
%
%  Get the spherical angles.
%
  [ a, b, c ] = sphere_triangle_sides_to_angles ( r, as, bs, cs );
%
%  Get the area.
%
  area = sphere_triangle_angles_to_area ( r, a, b, c );

  return
end
