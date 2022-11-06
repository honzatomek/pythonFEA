function circle_exp2imp_2d_test ( )

%*****************************************************************************80
%
%% circle_exp2imp_2d_test() tests circle_exp2imp_2d().
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license. 
%
%  Modified:
%
%    17 September 2020
%
%  Author:
%
%    John Burkardt
%
  fprintf ( 1, '\n' );
  fprintf ( 1, 'circle_exp2imp_2d_test():\n' );
  fprintf ( 1, '  circle_exp2imp_2d() computes the radius and \n' );
  fprintf ( 1, '  center of the circle through three points.\n' );
  fprintf ( 1, '\n' );
  fprintf ( 1, '  We can use this routine to compute, for three\n' );
  fprintf ( 1, '  points in space, the circle incident to those\n' );
  fprintf ( 1, '  points, and hence the radius of that circle,\n' );
  fprintf ( 1, '  and hence the curvature of those points.\n' );

  fprintf ( 1, '\n' );
  fprintf ( 1, '  Our three points are:\n' );
  fprintf ( 1, '\n' );
  fprintf ( 1, '    (0,0)\n' );
  fprintf ( 1, '    (1,0)\n' );
  fprintf ( 1, '    (C,S)\n' );
  fprintf ( 1, '\n' );
  fprintf ( 1, '  C = cosine ( theta), S = sine ( theta ).\n' );
  fprintf ( 1, '\n' );
  fprintf ( 1, '  Test     R           P3              PC        Theta    Curvature\n' );
  fprintf ( 1, '\n' );

  p1 = [ 0.0, 0.0 ];
  p2 = [ 1.0, 0.0 ];

  test_num = 13;

  for test = 1 : test_num

    theta = 2.0 * pi * ( test - 1 ) / ( test_num - 1 );

    theta_degrees = 360.0 * ( test - 1 ) / ( test_num - 1 );

    p3 = [ cos(theta), sin(theta) ];

    [ r, pc ] = circle_exp2imp_2d ( p1, p2, p3 );

    curvature = 1.0 / r;

    fprintf ( 1, '  %4d  %5.0g  (%6.0g,%6.0g)  (%6.0g,%6.0g)  %5.0f  %14.6g\n', ...
      test, r, p3(1:2), pc(1:2), theta_degrees, curvature );

  end

  return
end
