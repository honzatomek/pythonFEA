function voxels_dist_l1_nd_test ( )

%*****************************************************************************80
%
%% voxels_dist_l1_nd_test() tests voxels_dist_l1_nd().
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    23 May 2005
%
%  Author:
%
%    John Burkardt
%
  dim_num = 3;

  p1 = [ 1, 1, 5 ];
  p2 = [ 9, 4, 4 ];

  fprintf ( 1, '\n' );
  fprintf ( 1, 'voxels_dist_l1_nd_test():\n' );
  fprintf ( 1, '  voxels_dist_l1_nd() prints the voxels on a line in 3D.\n' );

  fprintf ( 1, '\n' );
  fprintf ( 1, '  P1:\n' );
  fprintf ( 1, '  %4d  %4d  %4d\n', p1(1:dim_num) );

  fprintf ( 1, '\n' );
  fprintf ( 1, '  P2:\n' );
  fprintf ( 1, '  %4d  %4d  %4d\n', p2(1:dim_num) );

  dist = voxels_dist_l1_nd ( dim_num, p1, p2 );
  fprintf ( 1, '\n' );
  fprintf ( 1, '  L1 distance = %d\n', dist );

  return
end
