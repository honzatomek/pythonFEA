function simplex_volume_nd_test ( )

%*****************************************************************************80
%
%% simplex_volume_nd_test() tests simplex_volume_nd().
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
  tetra = [ ...
     0.000000, -0.816496,  0.816496, 0.000000; ...
     0.942809, -0.816496, -0.816496, 0.000000; ...
    -0.333333, -0.333333, -0.333333, 1.000000 ];

  fprintf ( 1, '\n' );
  fprintf ( 1, 'simplex_volume_nd_test():\n' );
  fprintf ( 1, '  simplex_volume_nd() computes the volume of an ND simplex.\n' );

  r8mat_transpose_print ( 3, 4, tetra, '  Simplex vertices:' );

  volume = tetrahedron_volume ( tetra );

  fprintf ( 1, '\n' );
  fprintf ( 1, '  Volume computed by tetrahedron_volume_3d():\n' );
  fprintf ( 1, '  %f\n', volume );

  volume = simplex_volume_nd ( 3, tetra );

  fprintf ( 1, '\n' );
  fprintf ( 1, '  Volume computed by SIMPLEX_VOLUME_ND:\n' );
  fprintf ( 1, '  %f\n', volume );

  return
end
