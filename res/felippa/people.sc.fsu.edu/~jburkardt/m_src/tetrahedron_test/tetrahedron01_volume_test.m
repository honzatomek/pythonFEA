function tetrahedron01_volume_test ( )

%*****************************************************************************80
%
%% tetrahedron01_volume_test() tests tetrahedron01_volume().
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    16 January 2018
%
%  Author:
%
%    John Burkardt
%
  fprintf ( 1, '\n' );
  fprintf ( 1, 'tetrahedron01_volume_test():\n' );
  fprintf ( 1, '  tetrahedron01_volume() returns the volume of the unit tetrahedron.\n' );

  volume = tetrahedron01_volume ( );

  fprintf ( 1, '\n' );
  fprintf ( 1, '  Unit tetrahedron volume = %g\n', volume );

  return
end
