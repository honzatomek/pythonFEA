function volume = tetrahedron_volume ( tetra )

%*****************************************************************************80
%
%% tetrahedron_volume() computes the volume of a tetrahedron.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    25 April 2022
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
%    real VOLUME, the volume of the tetrahedron.
%
  A = zeros ( 4, 4 );
  A(1:3,1:4) = tetra(1:3,1:4);
  A(4,1:4) = 1.0;

  volume = abs ( det ( A ) ) / 6.0;

  return
end
