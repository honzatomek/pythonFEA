function geometry_test0183 ( )

%*****************************************************************************80
%
%% geometry_test0183() tests circle_llr2imp_2d().
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
  p_hi =  10.0;
  p_lo = -10.0;
  test_num = 5;

  fprintf ( 1, '\n' );
  fprintf ( 1, 'geometry_test0183()\n' );
  fprintf ( 1, '  circle_llr2imp_2d() is given:\n' );
  fprintf ( 1, '  a line through P1 and P2,\n' );
  fprintf ( 1, '  a line through Q1 and Q2,\n' );
  fprintf ( 1, '  and a radius R,\n' );
  fprintf ( 1, '  and determines the centers C of 4 circles\n' );
  fprintf ( 1, '  of the given radius, tangent to both lines.\n' );

  for test = 1 : test_num

    p1 = p_lo + ( p_hi - p_lo ) * rand ( 2, 1 );
    p2 = p_lo + ( p_hi - p_lo ) * rand ( 2, 1 );
    q1 = p_lo + ( p_hi - p_lo ) * rand ( 2, 1 );
    q2 = p_lo + ( p_hi - p_lo ) * rand ( 2, 1 );

    r_lo = 1.0;
    r_hi = 5.0;
    r = r_lo + ( r_hi - r_lo ) * rand ( 1, 1 );

    fprintf ( 1, '\n' );
    fprintf ( 1, '  Radius R = %f\n', r );

    fprintf ( 1, '  Point #P1: %f  %f\n', p1(1:2,1) );
    fprintf ( 1, '  Point #P2: %f  %f\n', p2(1:2,1) );
    fprintf ( 1, '  Point #Q1: %f  %f\n', q1(1:2,1) );
    fprintf ( 1, '  Point #Q2: %f  %f\n', q2(1:2,1) );

    pc = circle_llr2imp_2d ( p1, p2, q1, q2, r );

    fprintf ( 1, '  Center #1: %f  %f\n', pc(1:2,1) );
    fprintf ( 1, '  Center #2: %f  %f\n', pc(1:2,2) );
    fprintf ( 1, '  Center #1: %f  %f\n', pc(1:2,3) );
    fprintf ( 1, '  Center #2: %f  %f\n', pc(1:2,4) );

    d1 = line_exp_point_dist ( p1, p2, pc(1:2,1) );
    d2 = line_exp_point_dist ( p1, p2, pc(1:2,2) );
    d3 = line_exp_point_dist ( p1, p2, pc(1:2,3) );
    d4 = line_exp_point_dist ( p1, p2, pc(1:2,4) );

    fprintf ( 1, '  %f  %f  %f  %f\n', d1, d2, d3, d4 );

    d1 = line_exp_point_dist ( q1, q2, pc(1:2,1) );
    d2 = line_exp_point_dist ( q1, q2, pc(1:2,2) );
    d3 = line_exp_point_dist ( q1, q2, pc(1:2,3) );
    d4 = line_exp_point_dist ( q1, q2, pc(1:2,4) );

    fprintf ( 1, '  %f  %f  %f  %f\n', d1, d2, d3, d4 );

  end

  return
end
