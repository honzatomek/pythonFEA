function p = triangle_reference_sample ( n )

%*****************************************************************************80
%
%% triangle_reference_sample() returns random points in the reference triangle.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    09 November 2015
%
%  Author:
%
%    John Burkardt
%
%  Input:
%
%    integer N, the number of points to generate.
%
%  Output:
%
%    real P(2,N), random points in the triangle.
%
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

  beta = rand ( 1, n );

  p = zeros ( 2, n );

  p(1,1:n) = ( 1.0 - beta(1:n) ) .* alpha(1:n);
  p(2,1:n) =         beta(1:n)   .* alpha(1:n);

  return
end
