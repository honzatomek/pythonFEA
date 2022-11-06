function triangle_angles_3d_test ( )

%*****************************************************************************80
%
%% triangle_angles_3d_test() tests triangle_angles_3d();
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

  t = [ ...
    1.0,       2.0,       3.0; ...
    2.4142137, 3.4142137, 3.0; ...
    1.7071068, 2.7071068, 4.0 ]';

  fprintf ( 1, '\n' );
  fprintf ( 1, 'triangle_angles_3d_test():\n' );
  fprintf ( 1, '  triangle_angles_3d() computes the angles of a triangle\n' );
  fprintf ( 1, '  in 3D.\n' );

  r8mat_transpose_print ( dim_num, 3, t, '  Triangle vertices: ' );

  angle = triangle_angles_3d ( t );

  fprintf ( 1, '\n' );
  fprintf ( 1, '      Radians      Degrees\n' );
  fprintf ( 1, '\n' );

  for i = 1 : 3
    fprintf ( 1, '  %12f  %12f\n', angle(i), radians_to_degrees ( angle(i) ) );
  end

  return
end
