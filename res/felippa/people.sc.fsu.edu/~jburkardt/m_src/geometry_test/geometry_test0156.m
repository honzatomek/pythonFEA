function geometry_test0156 ( )

%*****************************************************************************80
%
%% geometry_test0156() tests circle_exp2imp_2d() and circle_imp2exp_2d().
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
  fprintf ( 1, '\n' );
  fprintf ( 1, 'geometry_test0156():\n' );
  fprintf ( 1, '  circle_exp2imp_2d() converts an explicit circle\n' );
  fprintf ( 1, '  to an implicit circle.\n' );
  fprintf ( 1, '  circle_imp2exp_2d() converts an implicit circle\n' );
  fprintf ( 1, '  to an explicit circle.\n' );

  pc1(1,1) = 10.0;
  pc1(2,1) =  5.0;
  r1 = 3.0;

  circle_imp_print_2d ( r1, pc1, '  The implicit circle:' );

  [ p1, p2, p3 ] = circle_imp2exp_2d ( r1, pc1 );

  r8vec_print ( 2, p1, '  P1:' );
  r8vec_print ( 2, p2, '  P2:' );
  r8vec_print ( 2, p3, '  P3:' );

  [ r2, pc2 ] = circle_exp2imp_2d ( p1, p2, p3 );

  circle_imp_print_2d ( r2, pc2, '  The recovered implicit circle:' );

  return
end
