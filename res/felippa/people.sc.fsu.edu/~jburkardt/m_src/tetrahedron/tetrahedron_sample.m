function p = tetrahedron_sample ( t, n )

%*****************************************************************************80
%
%% tetrahedron_sample() returns random points in a tetrahedron.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    12 November 2010
%
%  Author:
%
%    John Burkardt
%
%  Input:
%
%    real T(3,4), the tetrahedron vertices.
%
%    integer N, the number of points to generate.
%
%  Output:
%
%    real P(3,N), random points in the tetrahedron.
%
  for j = 1 : n

    r = rand ( 1, 1 );
%
%  Interpret R as a percentage of the tetrahedron's volume.
%
%  Imagine a plane, parallel to face 1, so that the volume between
%  vertex 1 and the plane is R percent of the full tetrahedron volume.
%
%  The plane will intersect sides 12, 13, and 14 at a fraction
%  ALPHA = R^1/3 of the distance from vertex 1 to vertices 2, 3, and 4.
%
    alpha = r .^ ( 1.0 / 3.0 );
%
%  Determine the coordinates of the points on sides 12, 13 and 14 intersected
%  by the plane, which form a triangle TR.
%
    tr(1:3,1) =         alpha   * t(1:3,1) ...
              + ( 1.0 - alpha ) * t(1:3,2);

    tr(1:3,2) =         alpha   * t(1:3,1) ...
              + ( 1.0 - alpha ) * t(1:3,3);

    tr(1:3,3) =         alpha   * t(1:3,1) ...
              + ( 1.0 - alpha ) * t(1:3,4);
%
%  Now choose, uniformly at random, a point in this triangle.
%
    r = rand ( 1, 1 );
%
%  Interpret R as a percentage of the triangle's area.
%
%  Imagine a line L, parallel to side 1, so that the area between
%  vertex 1 and line L is R percent of the full triangle's area.
%
%  The line L will intersect sides 2 and 3 at a fraction
%  ALPHA = SQRT ( R ) of the distance from vertex 1 to vertices 2 and 3.
%
    alpha = sqrt ( r );
%
%  Determine the coordinates of the points on sides 2 and 3 intersected
%  by line L.
%
    p12(1:3) =         alpha   * tr(1:3,1) ...
             + ( 1.0 - alpha ) * tr(1:3,2);

    p13(1:3) =         alpha   * tr(1:3,1) ...
             + ( 1.0 - alpha ) * tr(1:3,3);
%
%  Now choose, uniformly at random, a point on the line L.
%
    beta = rand ( 1, 1 );

    p(1:3,j) =         beta   * p12(1:3) ...
             + ( 1.0 - beta ) * p13(1:3);

  end

  return
end
