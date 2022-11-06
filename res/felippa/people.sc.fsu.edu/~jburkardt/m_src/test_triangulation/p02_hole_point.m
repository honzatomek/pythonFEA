function hole_point = p02_hole_point ( hole_index, m )

%*****************************************************************************80
%
%% p02_hole_point returns a point inside a given hole in problem 2.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    12 April 2009
%
%  Author:
%
%    John Burkardt
%
%  Input:
%
%    integer HOLE_INDEX, the index of the hole.
%
%    integer M, the spatial dimension.
%
%  Output:
%
%    real HOLE_point(M), a point in the hole
%
  center = [ 0.0, 0.0 ];
  hole_point(1:2) = center(1:2);

  return
end
