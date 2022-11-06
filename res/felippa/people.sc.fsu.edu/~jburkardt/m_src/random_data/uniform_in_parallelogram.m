function x = uniform_in_parallelogram ( n, v1, v2, v3 )

%*****************************************************************************80
%
%% uniform_in_parallelogram() maps uniform points into a parallelogram.
%
%  Discussion:
%
%    The parallelogram is defined by three vertices, V1, V2 and V3.
%    The missing vertex V4 is equal to V2+V3-V1.
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
%    real V1(2), V2(2), V3(2), the vertices.
%
%  Output:
%
%    real X(N,2), the points.
%
  x = zeros ( n, 2 );

  for i = 1 : n

    r = rand ( 2, 1 );

    x(i,1:2) = ( 1.0 - r(1) - r(2) ) * v1(1:2) ...
                     + r(1)          * v2(1:2) ...
                            + r(2)   * v3(1:2);

  end

  return
end
