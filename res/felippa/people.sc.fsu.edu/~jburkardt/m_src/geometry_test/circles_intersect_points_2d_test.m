function circles_intersect_points_2d_test ( )

%*****************************************************************************80
%
%% circles_intersect_points_2d_test() tests circles_intersect_points_2d();
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
  ntest = 5;
  center1(1:2,1) = [ 0.0; 0.0 ];
  r1 = 5.0;
  r2_test = [ 0.5, 5.0, 3.0, 3.0, 5.0 ];
  xc2_test = [ 5.0, 7.0710678, 4.0, 6.0, 0.0 ];
  yc2_test = [ 5.0, 7.0710678, 0.0, 0.0, 0.0 ];

  fprintf ( 1, '\n' );
  fprintf ( 1, 'circles_intersect_points_2d_test():\n' );
  fprintf ( 1, '  circles_intersect_points_2d() determines the intersections of\n' );
  fprintf ( 1, '  two circles in 2D.\n' );

  circle_imp_print_2d ( r1, center1, '  The first circle:' );

  for i = 1 : ntest

    r2 = r2_test(i);
    center2(1:2,1) = [ xc2_test(i); yc2_test(i) ];

    circle_imp_print_2d ( r2, center2, '  The second circle:' );

    [ num_int, x ] = circles_intersect_points_2d ( r1, center1, r2, center2 );

    if ( num_int == 0 )

      fprintf ( 1, '\n' );
      fprintf ( 1, '  The circles do not intersect.\n' );

    elseif ( num_int == 1 )

      fprintf ( 1, '\n' );
      fprintf ( 1, '  The circles intersect at one point:\n' );
      fprintf ( 1, '\n' );
      fprintf ( 1, '    X       Y\n' );
      fprintf ( 1, '\n' );
      fprintf ( 1, '  %6f  %6f \n', x(1:2,1) );

    elseif ( num_int == 2 )

      fprintf ( 1, '\n' );
      fprintf ( 1, '  The circles intersect at two points:\n' );
      fprintf ( 1, '\n' );
      fprintf ( 1, '    X       Y\n' );
      fprintf ( 1, '\n' );
      fprintf ( 1, '  %6f  %6f \n', x(1:2,1) );
      fprintf ( 1, '  %6f  %6f \n', x(1:2,2) );

    elseif ( num_int == 3 )

      fprintf ( 1, '\n' );
      fprintf ( 1, '  The circles coincide (infinite intersection).\n' );

    end

  end

  return
end
