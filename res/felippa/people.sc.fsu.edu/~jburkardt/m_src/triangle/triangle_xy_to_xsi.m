function xsi = triangle_xy_to_xsi ( t, p )

%*****************************************************************************80
%
%% triangle_xy_to_xsi() converts from XY to barycentric in 2D.
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
%    real P(2,1), the XY coordinates of a point.
%
%  Output:
%
%    real XSI(3,1), the barycentric coordinates of the point.
%    XSI1 + XSI2 + XSI3 should equal 1.
%

%
%  Destroy all row vectors!
%
  p = p(:);

  det = ( t(1,1) - t(1,3) ) * ( t(2,2) - t(2,3) ) ...
      - ( t(1,2) - t(1,3) ) * ( t(2,1) - t(2,3) );

  xsi(1,1) = (   ( t(2,2) - t(2,3) ) * ( p(1,1) - t(1,3) ) ...
               - ( t(1,2) - t(1,3) ) * ( p(2,1) - t(2,3) ) ) / det;

  xsi(2,1) = ( - ( t(2,1) - t(2,3) ) * ( p(1,1) - t(1,3) ) ...
               + ( t(1,1) - t(1,3) ) * ( p(2,1) - t(2,3) ) ) / det;

  xsi(3,1) = 1.0 - xsi(1,1) - xsi(2,1);

  return
end
