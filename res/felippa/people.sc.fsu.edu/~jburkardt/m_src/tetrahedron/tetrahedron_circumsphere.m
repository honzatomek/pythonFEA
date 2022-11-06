function [ r, center ] = tetrahedron_circumsphere ( tetra )

%*****************************************************************************80
%
%% tetrahedron_circumsphere() computes the circumsphere of a tetrahedron in 3D.
%
%  Discussion:
%
%    The circumsphere, or circumscribed sphere, of a tetrahedron is the sphere
%    that passes through the four vertices.  The circumsphere is not necessarily
%    the smallest sphere that contains the tetrahedron.
%
%    Surprisingly, the diameter of the sphere can be found by solving
%    a 3 by 3 linear system.  This is because the vectors P2 - P1,
%    P3 - P1 and P4 - P1 are secants of the sphere, and each forms a
%    right triangle with the diameter through P1.  Hence, the dot product of
%    P2 - P1 with that diameter is equal to the square of the length
%    of P2 - P1, and similarly for P3 - P1 and P4 - P1.  This determines
%    the diameter vector originating at P1, and hence the radius and
%    center.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    25 April 2022
%
%  Author:
%
%    John Burkardt
%
%  Reference:
%
%    Adrian Bowyer, John Woodwark,
%    A Programmer's Geometry,
%    Butterworths, 1983.
%
%  Input:
%
%    real TETRA(3,4) the tetrahedron vertices.
%
%  Output:
%
%    real R, CENTER(3,1), the center of the
%    circumscribed sphere, and its radius.  If the linear system is
%    singular, then R = -1, CENTER = 0.
%
  nrhs = 1;
%
%  Set up the linear system.
%
  A = ( tetra(1:3,2:4) )';

  for j = 1 : 3
    A(1:3,j) = A(1:3,j) - tetra(j,1);
  end

  x = zeros ( 3, 1 );
  for i = 1 : 3
    x(i) = sum ( A(i,1:3).^2 );
  end
%
%  Solve the linear system.
%
  c = A \ x;
%
%  Compute R, X, Y, Z.
%
  r = 0.5 * norm ( c );

  center(1:3,1) = tetra(1:3,1) + 0.5 * c(1:3,1);

  return
end
