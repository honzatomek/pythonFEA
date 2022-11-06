function c = tetrahedron_barycentric ( tetra, p )

%*****************************************************************************80
%
%% tetrahedron_barycentric() returns barycentric coordinates of a point.
%
%  Discussion:
%
%    The barycentric coordinates of a point (X,Y,Z) with respect to
%    a tetrahedron are a set of four values C(1:4), each associated
%    with a vertex of the tetrahedron.  The values must sum to 1.
%    If all the values are between 0 and 1, the point is contained
%    within the tetrahedron.
%
%    The barycentric coordinate of point X related to vertex A can be
%    interpreted as the ratio of the volume of the tetrahedron with 
%    vertex A replaced by vertex X to the volume of the original 
%    tetrahedron.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    12 August 2005
%
%  Author:
%
%    John Burkardt
%
%  Input:
%
%    real TETRA(3,4) the tetrahedron vertices.
%
%    real P(3,1), the point to be checked.
%
%  Output:
%
%    real C(4,1), the barycentric coordinates of (X,Y,Z) with
%    respect to the tetrahedron.
%

%
%  Set up the linear system
%
%    ( X2-X1  X3-X1  X4-X1 ) C1    X - X1
%    ( Y2-Y1  Y3-Y1  Y4-Y1 ) C2  = Y - Y1
%    ( Z2-Z1  Z3-Z1  Z4-Z1 ) C3    Z - Z1
%
%  which is satisfied by the barycentric coordinates of (X,Y,Z).
%
  p = p(:);

  A = tetra(1:3,2:4);
  for i = 1 : 3
    A(i,1:3) = A(i,1:3) - tetra(i,1);
  end

  x = p(1:3,1) - tetra(1:3,1);
%
%  Solve the linear system.
%
  c = A \ x;
  c0 = 1.0 - sum ( c(1:3) );
  c = [ c0; c ];

  return
end
