function geometry_test0185 ( )

%*****************************************************************************80
%
%% geometry_test0185() tests circle_pppr2imp_3d().
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    25 January 2019
%
%  Author:
%
%    John Burkardt
%
  dim_num = 3;
  p_hi =  10.0;
  p_lo = -10.0;
  test_num = 5;

  fprintf ( 1, '\n' );
  fprintf ( 1, 'geometry_test0185():\n' );
  fprintf ( 1, '  circle_pppr2imp_3d() is given 3D points P1, P2, P3,\n' );
  fprintf ( 1, '  and a radius R,\n' );
  fprintf ( 1, '  and determines the centers C of two circles\n' );
  fprintf ( 1, '  of the given radius, passing through P1 and P2\n' );
  fprintf ( 1, '  and lying in the plane of P1, P2 and P3.\n' );

  for test = 1 : test_num

    p1 = p_lo + ( p_hi - p_lo ) * rand ( dim_num, 1 );
    p2 = p_lo + ( p_hi - p_lo ) * rand ( dim_num, 1 );
    p3 = p_lo + ( p_hi - p_lo ) * rand ( dim_num, 1 );

    r_lo = sqrt ( sum ( ( p1(1:dim_num) - p2(1:dim_num) ).^2 ) );
    r_hi = r_lo + 5.0;
    r = r_lo + ( r_hi - r_lo ) * rand ( 1, 1 );

    fprintf ( 1, '\n' );
    fprintf ( 1, '  Radius R = %f\n', r );

    fprintf ( 1, '  Point #1: %f  %f  %f\n', p1(1:dim_num) );
    fprintf ( 1, '  Point #2: %f  %f  %f\n', p2(1:dim_num) );
    fprintf ( 1, '  Point #3: %f  %f  %f\n', p3(1:dim_num) );

    [ pc, normal ] = circle_pppr2imp_3d ( p1, p2, p3, r );

    fprintf ( 1, '  Center #1: %f  %f  %f\n', pc(1:dim_num,1) );
    fprintf ( 1, '  Center #2: %f  %f  %f\n', pc(1:dim_num,2) );
%
%  Check that the points are the right distance from the center.
%
    d11 = sqrt ( sum ( ( p1(1:dim_num) - pc(1:dim_num,1)' ).^2 ) );
    d21 = sqrt ( sum ( ( p2(1:dim_num) - pc(1:dim_num,1)' ).^2 ) );
    d12 = sqrt ( sum ( ( p1(1:dim_num) - pc(1:dim_num,2)' ).^2 ) );
    d22 = sqrt ( sum ( ( p2(1:dim_num) - pc(1:dim_num,2)' ).^2 ) );

    fprintf ( 1, '  %f  %f  %f  %f\n', d11, d21, d12, d22 );
%
%  Check that the radial vector to the point is perpendicular to NORMAL.
%
    d11 = normal(1:dim_num) * ( p1(1:dim_num) - pc(1:dim_num,1)' )';
    d21 = normal(1:dim_num) * ( p2(1:dim_num) - pc(1:dim_num,1)' )';
    d12 = normal(1:dim_num) * ( p1(1:dim_num) - pc(1:dim_num,2)' )';
    d22 = normal(1:dim_num) * ( p2(1:dim_num) - pc(1:dim_num,2)' )';

    fprintf ( 1, '  %f  %f  %f  %f\n', d11, d21, d12, d22 );

  end

  return
end
