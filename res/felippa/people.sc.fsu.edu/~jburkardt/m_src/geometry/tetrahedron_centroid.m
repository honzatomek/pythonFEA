function centroid = tetrahedron_centroid ( tetra )

%*****************************************************************************80
%
%% tetrahedron_centroid() computes the centroid of a tetrahedron.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    22 May 2005
%
%  Author:
%
%    John Burkardt
%
%  Input:
%
%    real TETRA(3,4) the tetrahedron vertices.
%
%  Output:
%
%    real CENTROID(3), the coordinates of the centroid.
%
  for i = 1 : 3
    centroid(i) = sum ( tetra(i,1:4) ) / 4.0;
  end

  return
end
