function p = triangle_xsi_to_xy ( t, xsi )

%*****************************************************************************80
%
%% triangle_xsi_to_xy() converts from barycentric to XY coordinates.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    05 December 2010
%
%  Author:
%
%    John Burkardt
%
%  Input:
%
%    real T(2,3), the triangle vertices.
%
%    real XSI(3,1), the barycentric coordinates of a point.
%    XSI(1) + XSI(2) + XSI(3) should equal 1, but this is not checked.
%
%  Output:
%
%    real P(2,1), the XY coordinates of the point.
%
  p(1:2,1) = t(1:2,1:3) * xsi(1:3,1);

  return
end
