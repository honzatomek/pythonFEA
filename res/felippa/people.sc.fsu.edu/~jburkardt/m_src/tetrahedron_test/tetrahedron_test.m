function tetrahedron_test ( )

%*****************************************************************************80
%
%% tetrahedron_test() tests tetrahedron().
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    11 May 2022
%
%  Author:
%
%    John Burkardt
%
  addpath ( '../tetrahedron' );

  timestamp ( );
  fprintf ( 1, '\n' );
  fprintf ( 1, 'tetrahedron_test():\n' );
  fprintf ( 1, '  MATLAB/Octave version %s\n', version ( ) );
  fprintf ( 1, '  tetrahedron() computes tetrahedron quantities.\n' );

  tetrahedron_barycentric_test ( );
  tetrahedron_centroid_test ( );
  tetrahedron_circumsphere_test ( );
  tetrahedron_contains_point_test ( );
  tetrahedron_dihedral_angles_test ( );
  tetrahedron_edge_length_test ( );
  tetrahedron_edges_test ( );
  tetrahedron_face_angles_test ( );
  tetrahedron_face_areas_test ( );
  tetrahedron_insphere_test ( );
  tetrahedron_lattice_layer_point_next_test ( );
  tetrahedron_lattice_point_next_test ( );
  tetrahedron_quality1_test();
  tetrahedron_quality2_test();
  tetrahedron_quality3_test();
  tetrahedron_quality4_test();
  tetrahedron_rhombic_shape_test ( );
  tetrahedron_sample_test ( );
  tetrahedron_shape_test ( );
  tetrahedron_solid_angles_test ( );
  tetrahedron_volume_test ( );

  tetrahedron01_lattice_point_num_test ( );
  tetrahedron01_volume_test ( );
%
%  Terminate.
%
  fprintf ( 1, '\n' );
  fprintf ( 1, 'tetrahedron_test():\n' );
  fprintf ( 1, '  Normal end of execution.\n' );
  fprintf ( 1, '\n' );
  timestamp ( );

  rmpath ( '../tetrahedron' );

  return
end
function timestamp ( )

%*****************************************************************************80
%
%% timestamp() prints the current YMDHMS date as a timestamp.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    14 February 2003
%
%  Author:
%
%    John Burkardt
%
  t = now;
  c = datevec ( t );
  s = datestr ( c, 0 );
  fprintf ( 1, '%s\n', s );

  return
end

