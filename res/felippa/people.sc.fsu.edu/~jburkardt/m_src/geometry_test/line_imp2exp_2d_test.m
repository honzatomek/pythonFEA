function line_imp2exp_2d_test ( )

%*****************************************************************************80
%
%% line_imp2exp_2d_test() tests line_imp2exp_2d().
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
  fprintf ( 1, '\n' );
  fprintf ( 1, 'line_imp2exp_2d_test():\n' );
  fprintf ( 1, '  line_imp2exp_2d() converts implicit to explicit lines.\n' );

  a = 1.0;
  b = 2.0;
  c = 3.0;

  fprintf ( 1, '\n' );
  fprintf ( 1, '  Implicit line A, B, C = %f  %f  %f\n', a, b, c );

  [ p1, p2 ] = line_imp2exp_2d ( a, b, c );

  r8vec_print ( 2, p1, '  The point P1:' );
  r8vec_print ( 2, p2, '  The point P2:' );

  [ a, b, c ] = line_exp2imp_2d ( p1, p2 );

  fprintf ( 1, '  Recovered A, B, C =  %f  %f  %f\n', a, b, c );

  return
end
