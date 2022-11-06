function sphere_grid_test09 ( )

%*****************************************************************************80
%
%% sphere_grid_test09() tests sphere_unit_sample().
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    30 August 2010
%
%  Author:
%
%    John Burkardt
%
  fprintf ( 1, '\n' );
  fprintf ( 1, 'SPHERE_GRID_TEST09\n' );
  fprintf ( 1, '  For the unit sphere in 3 dimensions:\n' );
  fprintf ( 1, '  SPHERE_UNIT_SAMPLE does a random sampling.\n' );

  node_num = 1000;

  node_xyz = sphere_unit_sample ( node_num );

  r8mat_transpose_print_some ( 3, node_num, node_xyz, 1, 1, 3, 10, ...
    '  First 10 values:' );
%
%  Write the nodes to a file.
%
  if ( true )

    file_name = sprintf ( 'sphere_sample_n%d.xyz', node_num );

    file_unit = fopen ( file_name, 'wt' );
    for node = 1 : node_num
      for i = 1 : 3
        fprintf ( file_unit, '  %f', node_xyz(i,node) );
      end
      fprintf ( file_unit, '\n' );
    end
    fclose ( file_unit );

    fprintf ( 1, '\n' );
    fprintf ( 1, '  Wrote grid nodes to "%s".\n', file_name );

  end

  return
end
