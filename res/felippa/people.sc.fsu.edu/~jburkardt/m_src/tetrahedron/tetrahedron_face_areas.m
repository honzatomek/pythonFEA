function areas = tetrahedron_face_areas ( tetra )

%*****************************************************************************80
%
%% tetrahedron_face_areas() returns the 4 face areas of a tetrahedron.
%
%  Discussion:
%
%    The tetrahedron has 4 triangular faces.  This routine computes the
%    area associated with each face.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    08 July 2009
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
%    real AREAS(4), the face areas.
%

%
%  Face 123
%
  tri(1:3,1:3) = tetra(1:3,1:3);
  areas(1) = triangle_area_3d ( tri );
%
%  Face 124
%
  tri(1:3,1:2) = tetra(1:3,1:2);
  tri(1:3,3) = tetra(1:3,4);
  areas(2) = triangle_area_3d ( tri );
%
%  Face 134
%
  tri(1:3,1) = tetra(1:3,1);
  tri(1:3,2:3) = tetra(1:3,3:4);
  areas(3) = triangle_area_3d ( tri );
%
%  Face 234
%
  tri(1:3,1:3) = tetra(1:3,2:4);
  areas(4) = triangle_area_3d ( tri );

  return
end
