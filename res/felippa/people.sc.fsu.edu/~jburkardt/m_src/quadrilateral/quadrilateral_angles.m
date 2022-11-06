function angles = quadrilateral_angles ( xy )

%*****************************************************************************80
%
%% quadrilateral_angles() computes the angles of a quadrilateral.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    01 August 2018
%
%  Author:
%
%    John Burkardt
%
%  Input:
%
%    real XY(2,4), the X and Y coordinates
%    of the corners of the quadrilateral.  The corners should be
%    specified in clockwise or counterclockwise order.
%
%  Output:
%
%    real ANGLES(4), the angles of the quadrilateral in radians.
%
  angles = polygon_angles ( 4, xy );

  return
end
