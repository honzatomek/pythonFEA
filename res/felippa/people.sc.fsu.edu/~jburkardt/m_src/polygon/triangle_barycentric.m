function xsi = triangle_barycentric ( t, p )

%*****************************************************************************80
%
%% triangle_barycentric() finds the barycentric coordinates of a point.
%
%  Discussion:
%
%    The barycentric coordinate of point X related to vertex A can be
%    interpreted as the ratio of the area of the triangle with
%    vertex A replaced by vertex X to the area of the original
%    triangle.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    05 Decewmber 2010
%
%  Author:
%
%    John Burkardt
%
%  Input:
%
%    real T(2,3), the triangle vertices.
%    The vertices should be given in counter clockwise order.
%
%    real P(2,1), the point to be checked.
%
%  Output:
%
%    real XSI(3,1), the barycentric coordinates of (X,Y)
%    with respect to the triangle.
%
  p = p(:);
%
%  Set up the linear system
%
%    ( X2-X1  X3-X1 ) XSI(1)  = X-X1
%    ( Y2-Y1  Y3-Y1 ) XSI(2)    Y-Y1
%
%  which is satisfied by the barycentric coordinates of (X,Y).
%
  A = zeros ( 2, 2 );
  b = zeros ( 2, 1 );

  A(1,1) = t(1,2) - t(1,1);
  A(1,2) = t(1,3) - t(1,1);
  b(1,1) = p(1,1) - t(1,1);

  A(2,1) = t(2,2) - t(2,1);
  A(2,2) = t(2,3) - t(2,1);
  b(2,1) = p(2,1) - t(2,1);
%
%  Solve the linear system.
%
  xsi = A \ b;
%
%  Append third coordinate.
%
  xsi(3,1) = 1.0 - xsi(1,1) - xsi(2,1);

  return
end
