function q = quadrilateral_random_convex ( )

%*****************************************************************************80
%
%% quadrilateral_random_convex() returns a random convex quadrilateral.
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
%    25 January 2019
%
%  Author:
%
%    John Burkardt
%
%  Output:
%
%    real Q(2,4), the coordinates of the
%    nodes of the quadrilateral, given in counterclockwise order.
%
  while ( true )
%
%  Generate a random quadrilateral.
%
    q_random = quadrilateral_random ( );
%
%  Break if the quadrilateral is convex.
%
    if ( quadrilateral_is_convex ( q_random ) )
      break
    end

  end
%
%  Determine the convex hull.
%
  [ hull_num, hull ] = convex_hull ( 4, q_random );
%
%  Make an ordered copy of the random points.
%
  q = zeros ( 2, 4 );
  for j = 1 : 4
    q(1:2,j) = q_random(1:2,hull(j));
  end

  return
end
