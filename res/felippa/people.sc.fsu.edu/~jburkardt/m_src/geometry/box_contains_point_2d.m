function value = box_contains_point_2d ( box, p )

%*****************************************************************************80
%
%% box_contains_point_2d() determines if a point is inside a box in 2D.
%
%  Discussion:
%
%    A box in 2D is a rectangle with sides aligned on coordinate
%    axes.  It can be described by its low and high corners, P1 and P2
%    as the set of points P satisfying:
%
%      BOX(1:2,1) <= P(1:2) <= BOX(1:2,2).
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    24 October 2006
%
%  Author:
%
%    John Burkardt
%
%  Input:
%
%    real BOX(2,2), the low and high corners of the box.
%
%    real P(2), the point to be checked.
%
%  Output:
%
%    logical VALUE, is TRUE if P is inside the box.
%
  value = false;

  for i = 1 : 2
    if ( p(i) < box(i,1) | box(i,2) < p(i) )
      return
    end
  end

  value = true;

  return
end
