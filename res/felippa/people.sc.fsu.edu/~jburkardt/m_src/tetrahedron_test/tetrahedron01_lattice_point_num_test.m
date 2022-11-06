function tetrahedron01_lattice_point_num_test ( )

%*****************************************************************************80
%
%% tetrahedron01_lattice_point_num_test() tests tetrahedron01_lattice_point_num().
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
  fprintf ( 1, '\n' );
  fprintf ( 1, 'tetrahedron01_lattice_point_num_test():\n' );
  fprintf ( 1, '  tetrahedron01_lattice_point_num() counts\n' );
  fprintf ( 1, '  lattice points inside the unit tetrahedron.\n' );

  fprintf ( 1, '\n' );
  for s = 0 : 10
    n = tetrahedron01_lattice_point_num ( s );
    fprintf ( 1, '  %6d  %6d\n', s, n );
  end

  return
end
