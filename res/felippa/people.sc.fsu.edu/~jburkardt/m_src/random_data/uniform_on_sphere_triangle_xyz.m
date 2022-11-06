function x = uniform_on_sphere_triangle_xyz ( n, v1, v2, v3 )

%*****************************************************************************80
%
%% uniform_on_sphere_triangle_xyz(): sample spherical triangle, XYZ coordinates.
%
%  Discussion:
%
%    The sphere has center 0 and radius 1.
%
%    A spherical triangle on the surface of the unit sphere contains those 
%    points with radius R = 1, bounded by the vertices V1, V2, V3.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license. 
%
%  Modified:
%
%    24 August 2010
%
%  Author:
%
%    John Burkardt
%
%  Reference:
%
%    James Arvo,
%    Stratified sampling of spherical triangles,
%    Computer Graphics Proceedings, Annual Conference Series, 
%    ACM SIGGRAPH '95, pages 437-438, 1995.
%
%  Input:
%
%    integer N, the number of points.
%
%    real V1(1,3), V2(1,3), V3(1,3), the XYZ coordinates of
%    the vertices of the spherical triangle.
%
%  Output:
%
%    real X(N,3), the XYZ coordinates of the 
%    sample points.
%
  x = zeros ( n, 3 );
%
%  Compute the sides, angles, and area of the spherical triangle;
%  for now, we assume R = 1.
%
  r = 1.0;

  [ a, b, c ] = stri_vertices_to_sides ( r, v1, v2, v3 );

  [ alpha, beta, gamma ] = stri_sides_to_angles ( r, a, b, c );

  area = stri_angles_to_area ( r, alpha, beta, gamma );

  for i = 1 : n
%
%  Select the new area.
%
    xsi1 = rand ( 1, 1 );
    area_hat = xsi1 * area;
%
%  Compute the sine and cosine of the angle phi.
%
    s = sin ( area_hat - alpha );
    t = cos ( area_hat - alpha );
%
%  Compute the pair that determines beta_hat.
%
    u = t - cos ( alpha );
    v = s + sin ( alpha ) * cos ( c );
%
%  Q is the cosine of the new edge length b_hat.
%
    q = ( ( v * t - u * s ) * cos ( alpha ) - v ) ...
      / ( ( v * s + u * t ) * sin ( alpha ) );
%
%  V31 = normalized ( V3 - ( V3 dot V1 ) * V1 )
%
    w = v3 * v1';
    v31 = v3 - w * v1;
    v31 = v31 / norm ( v31 );
%
%  V4 is the third vertex of the subtriangle V1, V2, V4.
%
    v4 = q * v1 + sqrt ( 1.0 - q * q ) * v31;
%
%  Select cos theta, which will sample along the edge from V2 to V4.
%
    xsi2 = rand ( 1, 1 );
    z = 1.0 - xsi2 * ( 1.0 - v4 * v2' );
%
%  V42 = normalized ( V4 - ( V4 dot V2 ) * V2 )
%
    w = v4 * v2';
    v42 = v4 - w * v2;
    v42 = v42 / norm ( v42 );
%
%  Construct the point.
%
    x(i,1:3) = z * v2 + sqrt ( 1.0 - z * z ) * v42;

  end

  return
end
