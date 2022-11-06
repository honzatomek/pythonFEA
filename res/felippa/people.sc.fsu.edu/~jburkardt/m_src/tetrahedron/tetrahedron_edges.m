function [ ab, ac, ad, bc, bd, cd ] = tetrahedron_edges ( tetra )

%*****************************************************************************80
%
%% tetrahedron_edges() computes the edges of a tetrahedron.
%
%  Discussion:
%
%    The vertices are A, B, C, D.  The edge from A to B is denoted by AB.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    11 May 2014
%
%  Author:
%
%    Original FORTRAN77 version by Barry Joe.
%    MATLAB version by John Burkardt.
%
%  Reference:
%
%    Barry Joe,
%    GEOMPACK - a software package for the generation of meshes
%    using geometric algorithms,
%    Advances in Engineering Software,
%    Volume 13, pages 325-331, 1991.
%
%  Input:
%
%    real TETRA(3,4), the vertices of the tetrahedron.
%
%  Output:
%
%    real AB(3), AC(3), AD(3), BC(3), BD(3), CD(3), the edges.
%
  ab(1:3) = tetra(1:3,2) - tetra(1:3,1);
  ac(1:3) = tetra(1:3,3) - tetra(1:3,1);
  ad(1:3) = tetra(1:3,4) - tetra(1:3,1);
  bc(1:3) = tetra(1:3,3) - tetra(1:3,2);
  bd(1:3) = tetra(1:3,4) - tetra(1:3,2);
  cd(1:3) = tetra(1:3,4) - tetra(1:3,3);

  return
end

