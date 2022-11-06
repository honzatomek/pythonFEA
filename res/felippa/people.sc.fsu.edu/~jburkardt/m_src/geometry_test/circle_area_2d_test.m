function circle_area_2d_test ( )

%*****************************************************************************80
%
%% circle_area_2d_test() tests circle_area_2d().
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    20 July 2018
%
%  Author:
%
%    John Burkardt
%
  fprintf ( 1, '\n' );
  fprintf ( 1, 'circle_area_2d_test():\n' );
  fprintf ( 1, '  circle_area_2d() computes the area of a circle of radius R.\n' );
  fprintf ( 1, '\n' );
  fprintf ( 1, '      R            Area\n' );
  fprintf ( 1, '\n' );

  r = 1.0;
  for i = 1 : 4
    area = circle_area_2d ( r );
    fprintf ( 1, '  %10f  %10f\n', r, area );
    r = r * 2;
  end

  return
end
