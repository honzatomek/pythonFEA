function dist = quadrilateral_point_dist ( q, p )

%*****************************************************************************80
%
%% quadrilateral_point_dist(): distance ( quadrilateral, point ).
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    04 December 2010
%
%  Author:
%
%    John Burkardt
%
%  Input:
%
%    real Q(2,4), the quadrilateral vertices.
%
%    real P(2,1), the point to be checked.
%
%  Output:
%
%    real DIST, the distance from the point to the quadrilateral.
%
  nside = 4;
%
%  Find the distance to each of the line segments.
%
  dist = Inf;

  for j = 1 : nside

    jp1 = i4_wrap ( j + 1, 1, nside );

    dist2 = segment_point_dist ( q(1:2,j), q(1:2,jp1), p(1:2,1) );

    if ( dist2 < dist )
      dist = dist2;
    end

  end

  return
end
