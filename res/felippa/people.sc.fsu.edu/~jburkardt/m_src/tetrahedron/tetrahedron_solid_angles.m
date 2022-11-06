function angle = tetrahedron_solid_angles ( tetra )

%*****************************************************************************80
%
%% tetrahedron_solid_angles() computes solid angles of a tetrahedron.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license. 
%
%  Modified:
%
%    07 July 2009
%
%  Author:
%
%    John Burkardt
%
%  Input:
%
%    real TETRA(3,4), the vertices of the tetrahedron.
%
%  Output:
%
%    real ANGLE(4), the solid angles.
%
  dihedral_angles = tetrahedron_dihedral_angles ( tetra );

  angle(1) = dihedral_angles(1) + dihedral_angles(2) + dihedral_angles(3) - pi;
  angle(2) = dihedral_angles(1) + dihedral_angles(4) + dihedral_angles(5) - pi;
  angle(3) = dihedral_angles(2) + dihedral_angles(4) + dihedral_angles(6) - pi;
  angle(4) = dihedral_angles(3) + dihedral_angles(5) + dihedral_angles(6) - pi;

  return
end
