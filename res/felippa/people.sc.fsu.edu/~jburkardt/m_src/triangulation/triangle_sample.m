function p = triangle_sample ( t, n )

%*****************************************************************************80
%
%% triangle_sample() returns random points in a triangle.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    10 April 2007
%
%  Author:
%
%    John Burkardt
%
%  Input:
%
%    real T(2,3), the triangle vertices.
%
%    integer N, the number of points to generate.
%
%  Output:
%
%    real P(2,N), random points in the triangle.
%
  dim_num = 2;

  alpha = rand ( 1, n );
%
%  Interpret R as a percentage of the triangle's area.
%
%  Imagine a line L, parallel to side 1, so that the area between
%  vertex 1 and line L is R percent of the full triangle's area.
%
%  The line L will intersect sides 2 and 3 at a fraction
%  ALPHA = SQRT ( R ) of the distance from vertex 1 to vertices 2 and 3.
%
  alpha(1:n) = sqrt ( alpha(1:n) );
%
%  Determine the coordinates of the points on sides 2 and 3 intersected
%  by line L.
%
  for dim = 1 : dim_num
    p12(dim,1:n) = ( 1.0 - alpha(1:n) ) * t(dim,1) ...
                         + alpha(1:n)   * t(dim,2);

    p13(dim,1:n) = ( 1.0 - alpha(1:n) ) * t(dim,1) ...
                         + alpha(1:n)   * t(dim,3);
  end
%
%  Now choose, uniformly at random, a point on the line L.
%
  alpha = rand ( 1, n );

  for dim = 1 : dim_num
    p(dim,1:n) = ( 1.0 - alpha(1:n) ) .* p12(dim,1:n) ...
                       + alpha(1:n)   .* p13(dim,1:n);
  end

 return
end
