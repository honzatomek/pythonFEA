function x = uniform_in_triangle ( n, v1, v2, v3 )

%*****************************************************************************80
%
%% uniform_in_triangle() maps uniform points into a triangle.
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
%    13 April 2022
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
%    integer N, the number of points.
%
%    real V1(1,2), V2(1,2), V3(1,2), the vertices.
%
%  Output:
%
%    real X(N,2), the points.
%
  x = zeros ( n, 2 );
%
%  Generate the points using Turk's rule 1.
%
  for i = 1 : n

    r = rand ( 1, 2 );

    a = 1.0 - sqrt ( r(2) );
    b = ( 1.0 - r(1) ) * sqrt ( r(2) );
    c = r(1) * sqrt ( r(2) );

    x(i,1:2) = ( a * v1(1,1:2) ...
               + b * v2(1,1:2) ...
               + c * v3(1,1:2) );

  end

  return
end
