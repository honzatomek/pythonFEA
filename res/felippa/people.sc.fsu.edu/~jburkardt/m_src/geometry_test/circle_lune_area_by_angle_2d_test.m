function circle_lune_area_by_angle_2d_test ( )

%*****************************************************************************80
%
%% circle_lune_area_by_angle_2d_test() tests circle_lune_area_by_angle_2d().
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    14 January 2018
%
%  Author:
%
%    John Burkardt
%
  n_test = 12;

  center(1:2) = [ 0.0, 0.0 ];
  r = 2.0;

  fprintf ( 1, '\n' );
  fprintf ( 1, 'circle_lune_area_by_angle_2d_test():\n' );
  fprintf ( 1, '  circle_lune_area_by_angle_2d() computes the area of a\n' );
  fprintf ( 1, '  circular lune, defined by joining the endpoints\n' );
  fprintf ( 1, '  of a circular arc.\n' );
  fprintf ( 1, '\n' );
  fprintf ( 1, '      R            Theta1      Theta2        Area\n' );
  fprintf ( 1, '\n' );

  for i = 0 : n_test

    theta1 = 0.0;
    theta2 = i * 2.0 * pi / n_test;

    area = circle_lune_area_by_angle_2d ( r, center, theta1, theta2 );

    fprintf ( 1, '  %10f  %10f  %10f  %10f\n', r, theta1, theta2, area );

  end

  return
end
