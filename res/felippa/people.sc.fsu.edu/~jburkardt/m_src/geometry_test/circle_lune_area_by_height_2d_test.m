function circle_lune_area_by_height_2d_test ( )

%*****************************************************************************80
%
%% circle_lune_area_by_height_2d_test() tests circle_lune_area_by_height_2d().
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
  n_test = 6;

  r = 2.0;

  fprintf ( 1, '\n' );
  fprintf ( 1, 'circle_lune_area_by_height_2d_test():\n' );
  fprintf ( 1, '  circle_lune_area_by_height_2d() computes the area of a\n' );
  fprintf ( 1, '  circular lune based on the "height" of the circular triangle.\n' );
  fprintf ( 1, '\n' );
  fprintf ( 1, '      R            H        Area\n' );
  fprintf ( 1, '\n' );

  for i = - n_test : n_test

    h = i * r / n_test;

    area = circle_lune_area_by_height_2d ( r, h );

    fprintf ( 1, '  %10f  %10f  %10f\n', r, h, area );

  end

  return
end
