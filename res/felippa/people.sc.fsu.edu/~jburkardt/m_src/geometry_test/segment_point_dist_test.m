function segment_point_dist_test ( )

%*****************************************************************************80
%
%% segment_point_dist_test() tests segment_point_dist().
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
  test_num = 3;

  fprintf ( 1, '\n' );
  fprintf ( 1, 'segment_point_dist_test():\n' );
  fprintf ( 1, '  segment_point_dist() computes the distance\n' );
  fprintf ( 1, '  between a line segment and point in 2D.\n' );

  for test = 1 : test_num

    p1 = rand ( 2, 1 );
    p2 = rand ( 2, 1 );
    p = rand ( 2, 1 );

    dist = segment_point_dist ( p1, p2, p );

    fprintf ( 1, '\n' );
    fprintf ( 1, '  TEST = %d', test );
    fprintf ( 1, '  P1 =   %12f  %12f\n', p1(1:2,1) );
    fprintf ( 1, '  P2 =   %12f  %12f\n', p2(1:2,1) );
    fprintf ( 1, '  P =    %12f  %12f\n', p(1:2,1) );
    fprintf ( 1, '  DIST = %12f\n', dist );

  end

  return
end
