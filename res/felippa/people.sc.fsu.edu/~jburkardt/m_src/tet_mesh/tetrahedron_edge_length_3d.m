function edge_length = tetrahedron_edge_length_3d ( tetra )

%*****************************************************************************80
%
%% tetrahedron_edge_length_3d() returns edge lengths of a tetrahedron in 3D.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    29 May 2010
%
%  Author:
%
%    John Burkardt
%
%  Input:
%
%    real TETRA(3,4), the tetrahedron vertices.
%
%  Output:
%
%    real EDGE_LENGTH(6,1), the length of the edges.
%
  dim_num = 3;
  edge_length = zeros ( 6, 1 );

  k = 0;
  for j1 = 1 : 3
    for j2 = j1+1 : 4
      k = k + 1;
      edge_length(k) = norm ( tetra(1:dim_num,j2) - tetra(1:dim_num,j1) );
    end
  end

  return
end
