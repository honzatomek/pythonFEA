function geometry_test019 ( )

%*****************************************************************************80
%
%% geometry_test019() tests circle_ppr2imp_2d().
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
  dim_num = 2;

  p_hi =  10.0;
  p_lo = -10.0;
  test_num = 5;

  fprintf ( 1, '\n' );
  fprintf ( 1, 'geometry_test019():\n' );
  fprintf ( 1, '  circle_ppr2imp_2d() is given points P1 and P2,\n' );
  fprintf ( 1, '  and a radius R,\n' );
  fprintf ( 1, '  and determines the centers C of two circles\n' );
  fprintf ( 1, '  of the given radius, passing through P1 and P2.\n' );

  for test = 1 : test_num

    p1 = p_lo + ( p_hi - p_lo ) * rand ( dim_num, 1 );
    p2 = p_lo + ( p_hi - p_lo ) * rand ( dim_num, 1 );

    r_lo = sqrt ( sum ( ( p1(1:dim_num) - p2(1:dim_num) ).^2 ) );
    r_hi = r_lo + 5.0;
    r = r_lo + ( r_hi - r_lo ) * rand ( 1, 1 );

    fprintf ( 1, '\n' );
    fprintf ( 1, '  Radius R = %f\n', r );

    fprintf ( 1, '  Point #1: %f  %f\n', p1(1:dim_num) );
    fprintf ( 1, '  Point #2: %f  %f\n', p2(1:dim_num) );

    pc = circle_ppr2imp_2d ( p1, p2, r );

    fprintf ( 1, '  Center #1: %f  %f\n', pc(1:dim_num,1) );
    fprintf ( 1, '  Center #2: %f  %f\n', pc(1:dim_num,2) );

    d11 = sqrt ( sum ( ( p1(1:dim_num) - pc(1:dim_num,1)' ).^2 ) );
    d21 = sqrt ( sum ( ( p2(1:dim_num) - pc(1:dim_num,1)' ).^2 ) );
    d12 = sqrt ( sum ( ( p1(1:dim_num) - pc(1:dim_num,2)' ).^2 ) );
    d22 = sqrt ( sum ( ( p2(1:dim_num) - pc(1:dim_num,2)' ).^2 ) );

    fprintf ( 1, '  %f  %f  %f  %f\n', d11, d21, d12, d22 );

  end

  return
end
