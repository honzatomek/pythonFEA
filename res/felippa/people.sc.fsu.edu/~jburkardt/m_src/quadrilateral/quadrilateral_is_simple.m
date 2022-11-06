function value = quadrilateral_is_simple ( xy )

%*****************************************************************************80
%
%% quadrilateral_is_simple() determines whether a quadrilateral is simple.
%
%  Discussion:
%
%    A simple quadrilateral is one that is non-degenerate.
%
%    Visually speaking, a degenerate quadrilateral is one in which
%    one side crosses another; the shape looks twisted or folded.
%    Angles and areas and centroids and similar quantities
%    become difficult to define or compute for degenerate quadrilaterals.
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
%    real XY(2,4), the coordinates of the
%    nodes of the quadrilateral, given in counterclockwise order.
%
%  Output:
%
%    logical VALUE, is TRUE if the quadrilateral is simple.
%
  angles = quadrilateral_angles ( xy );
  angle_sum = sum ( angles );
%
%  A degenerate quadrilateral would typically have an angle sum of 4 pi
%  degrees, so this test could be loosened.
%
  value = ( abs ( angle_sum - 2.0 * pi ) < 100.0 * eps );

  return
end
