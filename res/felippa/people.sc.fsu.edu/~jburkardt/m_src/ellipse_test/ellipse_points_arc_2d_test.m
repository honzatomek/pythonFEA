function ellipse_points_arc_2d_test ( )

%*****************************************************************************80
%
%% ellipse_points_arc_2d_test() tests ellipse_points_arc_2d().
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    30 July 2018
%
%  Author:
%
%    John Burkardt
%
  n = 13;

  center = [ 5.0, -2.0 ];
  r1 = 3.0;
  r2 = 1.0;
  psi = pi / 6.0;
  theta1 = pi / 2.0;
  theta2 = 2.0 * pi;

  fprintf ( 1, '\n' );
  fprintf ( 1, 'ellipse_points_arc_2d_test():\n' );
  fprintf ( 1, '  ellipse_points_arc_2d() returns points on an\n' );
  fprintf ( 1,'   elliptical arc.\n' );
  fprintf ( 1, '\n' );
  fprintf ( 1, '  The ellipse has center %f  %f\n', center(1:2) );
  fprintf ( 1, '  radii R1 = %f, R2 = %f\n', r1, r2 );
  fprintf ( 1, '  and angle PSI = %f\n', psi );
  fprintf ( 1, '\n' );
  fprintf ( 1, '  The arc extends from THETA1 = %f\n', theta1 );
  fprintf ( 1, '  to THETA2 = %f\n', theta2 );

  p = ellipse_points_arc_2d ( center, r1, r2, psi, theta1, theta2, n );

  fprintf ( 1, '  Sample points:\n' );
  disp ( p' );

  return
end
