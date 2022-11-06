function line_exp_perp_2d_test ( )

%*****************************************************************************80
%
%% line_exp_perp_2d_test() tests line_exp_perp_2d().
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    31 July 2009
%
%  Author:
%
%    John Burkardt
%
  ntest = 3;

  fprintf ( 1, '\n' );
  fprintf ( 1, 'line_exp_perp_2d_test():\n' );
  fprintf ( 1, '  line_exp_perp_2d() is given an explicit line (P1,P2),\n' );
  fprintf ( 1, '  and another point P3.  It then finds a point\n' );
  fprintf ( 1, '  P4 on (P1,P2) so that (P1,P2) is perpendicular\n' );
  fprintf ( 1, '  to (P3,P4).\n' );

  p1(1:2) = [ 1.0, 3.0 ];
  p2(1:2) = [ 4.0, 0.0 ];

  p3test(1:2,1:ntest) = [ ...
    0.0,  0.0; ...
    5.0, -1.0; ...
    5.0,  3.0 ]';

  r8vec_print ( 2, p1, '  Point P1:' );
  r8vec_print ( 2, p2, '  Point P2:' );

  for j = 1 : ntest

    p3(1:2) = p3test(1:2,j);

    r8vec_print ( 2, p3, '  Point P3:' );

    [ p4, flag ] = line_exp_perp_2d ( p1, p2, p3 );

    r8vec_print ( 2, p4, '  Point P4:' );

  end

  return
end
