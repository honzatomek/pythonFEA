function voxels_line_3d_test ( )

%*****************************************************************************80
%
%% voxels_line_3d_test() tests voxels_line_3d().
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    31 July 2018
%
%  Author:
%
%    John Burkardt
%
  p1 = [ 1, 1, 5 ];
  p2 = [ 9, 4, 4 ];

  fprintf ( 1, '\n' );
  fprintf ( 1, 'voxels_line_3d_test():\n' );
  fprintf ( 1, '  voxels_line_3d() computes the voxels on a line in 3D\n' );
  fprintf ( 1, '  starting at the first voxel, and heading towards\n' );
  fprintf ( 1, '  the second one.\n' );

  fprintf ( 1, '\n' );
  fprintf ( 1, '  Starting voxel:\n' );
  fprintf ( 1, '  %6d  %6d  %6d\n', p1(1:3) );

  fprintf ( 1, '\n' );
  fprintf ( 1, '  "Heading" voxel:\n' );
  fprintf ( 1, '  %6d  %6d  %6d\n', p2(1:3) );

  n = voxels_dist_l1_nd ( 3, p1, p2 ) + 1;

  fprintf ( 1, '\n' );
  fprintf ( 1, '  Number of voxels we will compute is %d\n', n );

  v = voxels_line_3d ( p1, p2, n );

  i4mat_transpose_print ( 3, n, v, '  The voxels:' );

  return
end
