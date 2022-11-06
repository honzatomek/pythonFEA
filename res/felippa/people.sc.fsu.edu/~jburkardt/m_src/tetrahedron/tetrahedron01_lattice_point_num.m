function n = tetrahedron01_lattice_point_num ( s )

%*****************************************************************************80
%
%% tetrahedron01_lattice_point_num() counts lattice points.
%
%  Discussion:
%
%    The tetrahedron is assumed to be the unit tetrahedron:
%
%    ( (0,0,0), (1,0,0), (0,1,0), (0,0,1) )
%
%    or a copy of this tetrahedron scaled by an integer S:
%
%    ( (0,0,0), (S,0,0), (0,S,0), (0,0,S) ).
%
%    The routine returns the number of integer lattice points that appear
%    inside the tetrahedron, or on its faces, edges or vertices.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    03 July 2009
%
%  Author:
%
%    John Burkardt
%
%  Reference:
%
%    Matthias Beck, Sinai Robins,
%    Computing the Continuous Discretely,
%    Springer, 2006,
%    ISBN13: 978-0387291390,
%    LC: QA640.7.B43.
%
%  Input:
%
%    integer S, the scale factor, 0 <= S.
%
%  Output:
%
%    integer N, the number of lattice points.
%
  n = ( ( s + 3 ) * ( s + 2 ) * ( s + 1 ) ) / 6;

  return
end
