function circle_lune_height_by_angle_2d_test ( )

%*****************************************************************************80
%
%% circle_lune_height_by_angle_2d_test() tests circle_lune_height_by_angle_2d().
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

  r = 2.0;

  fprintf ( 1, '\n' );
  fprintf ( 1, 'circle_lune_height_by_angle_2d_test():\n' );
  fprintf ( 1, '  circle_lune_height_by_angle_2d() computes the height of\n' );
  fprintf ( 1, '  the triangle of a circular lune, given the subtended angle.\n' );
  fprintf ( 1, '\n' );
  fprintf ( 1, '      R            Angle        Height\n' );
  fprintf ( 1, '\n' );

  for i = 0 : n_test

    angle = i * 2.0 * pi / n_test;

    height = circle_lune_height_by_angle_2d ( r, angle );

    fprintf ( 1, '  %10f  %10f  %10f\n', r, angle, height );

  end

  return
end
