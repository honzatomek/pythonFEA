function x = uniform_in_triangle_map1 ( v1, v2, v3, n )

%*****************************************************************************80
%
%% uniform_in_triangle_map1() maps uniform points into a triangle.
%
%  Discussion:
%
%    The triangle is defined by three vertices.  This routine
%    uses Turk's rule #1.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license. 
%
%  Modified:
%
%    17 January 2016
%
%  Author:
%
%    John Burkardt
%
%  Reference:
%
%    Greg Turk,
%    Generating Random Points in a Triangle,
%    in Graphics Gems,
%    edited by Andrew Glassner,
%    AP Professional, 1990, pages 24-28.
%
%  Input:
%
%    real V1(2), V2(2), V3(2), the vertices.
%
%    integer N, the number of points.
%
%  Output:
%
%    real X(2,N), the points.
%

%
%  Destroy all row vectors.
%
  v1 = v1(:);
  v2 = v2(:);
  v3 = v3(:);

  x = zeros ( 2, n );
%
%  Generate the points using Turk's rule 1.
%
  for j = 1 : n

    r = rand ( 2, 1 );

    a = 1.0 - sqrt ( r(2) );
    b = ( 1.0 - r(1) ) * sqrt ( r(2) );
    c = r(1) * sqrt ( r(2) );

    x(1:2,j) = ( a * v1(1:2) ...
               + b * v2(1:2) ...
               + c * v3(1:2) )';

  end

  return
end
