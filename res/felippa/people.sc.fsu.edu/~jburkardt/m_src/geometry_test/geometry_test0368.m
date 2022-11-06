function geometry_test0368 ( )

%*****************************************************************************80
%
%% geometry_test0368() tests segment_point_dist_3d().
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    14 February 2003
%
%  Author:
%
%    John Burkardt
%
  dim_num = 3;
  test_num = 3;
 
  fprintf ( 1, '\n' );
  fprintf ( 1, 'geometry_test0368():\n' );
  fprintf ( 1, '  segment_point_near_3d() computes the nearest point\n' );
  fprintf ( 1, '  from a line segment to a point in 3D.\n' );

  for test = 1 : test_num

    p1 = rand ( dim_num, 1 );
    p2 = rand ( dim_num, 1 );
    p = rand ( dim_num, 1 );

    [ pn, dist, t ] = segment_point_near_3d ( p1, p2, p );

    fprintf ( 1, '\n' );
    fprintf ( 1, '  TEST = %d', test );
    fprintf ( 1, '  P1 =   %12f  %12f  %12f\n', p1(1:dim_num) );
    fprintf ( 1, '  P2 =   %12f  %12f  %12f\n', p2(1:dim_num) );
    fprintf ( 1, '  P =    %12f  %12f  %12f\n', p(1:dim_num) );
    fprintf ( 1, '  PN =   %12f  %12f  %12f\n', pn(1:dim_num) );
    fprintf ( 1, '  DIST = %12f\n', dist );
    fprintf ( 1, '  T =    %12f\n', t );

  end

  return
end
