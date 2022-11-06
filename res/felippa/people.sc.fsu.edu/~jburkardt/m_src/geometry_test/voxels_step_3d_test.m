function voxels_step_3d_test ( )

%*****************************************************************************80
%
%% voxels_step_3d_test() tests voxels_step_3d().
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    18 February 2009
%
%  Author:
%
%    John Burkardt
%
  fprintf ( 1, '\n' );
  fprintf ( 1, 'voxels_step_3d_test():\n' );
  fprintf ( 1, '  voxels_step_3d() steps along a line from\n' );
  fprintf ( 1, '  one voxel to another.\n' );

  v1(1:3) = [ 1, 1, 5 ];
  v2(1:3) = v1(1:3);

  inc = 7;
  jnc = 3;
  knc = -1;

  fprintf ( 1, '\n' );
  fprintf ( 1, '  %4d  %4d  %4d  %4d\n', 0, v2(1:3) );

  for i = 1 : 10
    v3 = voxels_step_3d ( v1, v2, inc, jnc, knc );
    fprintf ( 1, '  %4d  %4d  %4d  %4d\n', i, v3(1:3) );
    v2(1:3) = v3(1:3);
  end

  fprintf ( 1, '\n' );
  fprintf ( 1, '  Now, as a check, reverse direction and return.\n' );
  fprintf ( 1, '\n' );

  v1(1:3) = v2(1:3);

  inc = -inc;
  jnc = -jnc;
  knc = -knc;

  v2(1:3) = v1(1:3);

  fprintf ( 1, '  %4d  %4d  %4d  %4d\n', 0, v2(1:3) );
  for i = 1 : 10
    v3 = voxels_step_3d ( v1, v2, inc, jnc, knc );
    fprintf ( 1, '  %4d  %4d  %4d  %4d\n', i, v3(1:3) );
    v2(1:3) = v3(1:3);
  end

  return
end
