function segment_point_near_test ( )

%*****************************************************************************80
%
%% segment_point_near_test() tests segment_point_near().
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    21 July 2018
%
%  Author:
%
%    John Burkardt
%
  test_num = 3;

  fprintf ( 1, '\n' );
  fprintf ( 1, 'segment_point_near_test():\n' );
  fprintf ( 1, '  segment_point_near() computes the nearest point\n' );
  fprintf ( 1, '  from a line segment to a point.\n' );

  for test = 1 : test_num

    p1 = rand ( 2, 1 );
    p2 = rand ( 2, 1 );
    p = rand ( 2, 1 );

    [ pn, dist, t ] = segment_point_near ( p1, p2, p );

    fprintf ( 1, '\n' );
    fprintf ( 1, '  TEST = %d', test );
    fprintf ( 1, '  P1 =   %12f  %12f\n', p1(1:2) );
    fprintf ( 1, '  P2 =   %12f  %12f\n', p2(1:2) );
    fprintf ( 1, '  P =    %12f  %12f\n', p(1:2) );
    fprintf ( 1, '  PN =   %12f  %12f\n', pn(1:2) );
    fprintf ( 1, '  DIST = %12f\n', dist );
    fprintf ( 1, '  T =    %12f\n', t );

  end

  return
end
