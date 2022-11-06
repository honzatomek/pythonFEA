function value = quadrilateral_is_convex ( xy )

%*****************************************************************************80
%
%% quadrilateral_is_convex() determines whether a quadrilateral is convex.
%
%  Discussion:
%
%    Actually, this function ignores the ordering of the nodes.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    08 April 2019
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
%    logical VALUE, is TRUE if the quadrilateral is convex.
%
  angles = quadrilateral_angles ( xy );
  angle_sum = sum ( angles(1:4) );

  value = ( ...
    all ( 0.0 < angles(1:4)         ) & ...
    all (       angles(1:4) < pi ) & ...
        ( abs ( angle_sum - 2.0 * pi ) < 1.0 ) );

  return
end
