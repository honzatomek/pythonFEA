function circles_intersect_area_2d_test ( )

%*****************************************************************************80
%
%% circles_intersect_area_2d_test() tests circles_intersect_area_2d();
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
  ntest = 6;
  r1_test = [ 1.0, 1.0, 1.0, 1.0, 1.0, 1.0 ];
  r2_test = [ 0.5, 0.5, 0.5, 1.0, 1.0, 1.0 ];
  d_test =  [ 1.5, 1.0, 0.5, 1.5, 1.0, 0.0 ];

  fprintf ( 1, '\n' );
  fprintf ( 1, 'circles_intersect_area_2d_test():\n' );
  fprintf ( 1, '  circles_intersect_area_2d() determines the area of the\n' );
  fprintf ( 1, '  intersection of two circes of radius R1 and R2,\n' );
  fprintf ( 1, '  with a distance D between the centers.\n' );
  fprintf ( 1, '\n' );
  fprintf ( 1, '      R1      R2       D    Area\n' );
  fprintf ( 1, '\n' );

  for i = 1 : ntest

    r1 = r1_test(i);
    r2 = r2_test(i);
    d = d_test(i);
    area = circles_intersect_area_2d ( r1, r2, d );

    fprintf ( 1, '  %6f  %6f  %6f  %6f\n', r1, r2, d, area );

  end

  return
end
