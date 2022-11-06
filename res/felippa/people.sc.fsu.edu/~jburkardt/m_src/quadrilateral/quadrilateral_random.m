function xy = quadrilateral_random ( )

%*****************************************************************************80
%
%% quadrilateral_random() returns a random quadrilateral.
%
%  Description:
%
%    The quadrilateral is constrained in that the vertices must all lie
%    with the unit square.
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
%  Output:
%
%    real XY(2,4), the coordinates of the nodes of the quadrilateral.
%
  xy = rand ( 2, 4 );

  return
end
