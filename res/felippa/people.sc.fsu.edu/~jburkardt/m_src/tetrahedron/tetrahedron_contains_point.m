function inside = tetrahedron_contains_point ( tetra, p )

%*****************************************************************************80
%
%% tetrahedron_contains_point() finds if a point is inside a tetrahedron.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    30 January 2005
%
%  Author:
%
%    John Burkardt
%
%  Input:
%
%    real TETRA(3,4), the tetrahedron vertices.
%
%    real P(3), the point to be checked.
%
%  Output:
%
%    logical INSIDE, is TRUE if (X,Y,Z) is inside
%    the tetrahedron or on its boundary.
%
  c = tetrahedron_barycentric ( tetra, p );
%
%  If the point is in the tetrahedron, its barycentric coordinates
%  must be nonnegative.
%
  if ( any ( c(1:4) < 0.0 ) )
    inside = false;
  else
    inside = true;
  end

  return
end
