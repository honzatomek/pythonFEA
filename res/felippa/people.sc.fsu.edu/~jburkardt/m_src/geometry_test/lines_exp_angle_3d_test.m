function lines_exp_angle_3d_test ( )

%*****************************************************************************80
%
%% lines_exp_angle_3d_test() tests lines_exp_angle_3d();
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
  dim_num = 3;

  fprintf ( 1, '\n' );
  fprintf ( 1, 'lines_exp_angle_3d_test():\n' );
  fprintf ( 1, '  lines_exp_angle_3d() finds the angle between\n' );
  fprintf ( 1, '  two explicit lines in 3D;\n' );

  p1(1:dim_num) = [ 0.0, 0.0, 0.0 ];
  p2(1:dim_num) = [ 1.0, 2.0, 0.0 ];

  q1(1:dim_num) = [ 0.0, 3.0, 3.0 ];
  q2(1:dim_num) = [ 3.0, 0.0, 3.0 ];

  angle = lines_exp_angle_3d ( p1, p2, q1, q2 );

  fprintf ( 1, '\n' );
  fprintf ( 1, '  Angle between lines is %f\n', angle );

  p1(1:dim_num) = [ 0.0, 0.0, 0.0 ];
  p2(1:dim_num) = [ 1.0, 2.0, 0.0 ];

  q1(1:dim_num) = [ 1.0, 2.0, -1.0 ];
  q2(1:dim_num) = [ 1.0, 2.0,  3.0 ];

  angle = lines_exp_angle_3d ( p1, p2, q1, q2 );

  fprintf ( 1, '\n' );
  fprintf ( 1, '  Angle between lines is %f\n', angle );

  return
end
