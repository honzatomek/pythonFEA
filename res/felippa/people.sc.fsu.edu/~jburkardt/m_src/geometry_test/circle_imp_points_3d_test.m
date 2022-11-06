function circle_imp_points_3d_test ( )

%*****************************************************************************80
%
%% circle_imp_points_3d_test() tests circle_imp_points_3d().
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    04 December 2010
%
%  Author:
%
%    John Burkardt
%
  r = 2.0;
  pc = [ 5.0; -2.0; 1.0 ];
  nc = [ 1.0; 1.0; 1.0 ];
  n = 12;

  fprintf ( 1, '\n' );
  fprintf ( 1, 'circle_imp_points_3d_test():\n' );
  fprintf ( 1, '  circle_imp_points_3d() gets points on a circle in 3D;\n' );

  circle_imp_print_3d ( r, pc, nc, '  The implicit circle:' );

  p = circle_imp_points_3d ( r, pc, nc, n );

  r8mat_transpose_print ( 3, n, p, '  Points on the circle:' );

  return
end
