function circle_imp_print_2d_test ( )

%*****************************************************************************80
%
%% circle_imp_print_2d_test() tests circle_imp_print_2d().
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    15 January 2018
%
%  Author:
%
%    John Burkardt
%
  center = [ 5.0; -2.0 ];
  r = 2.0;
 
  fprintf ( 1, '\n' );
  fprintf ( 1, 'circle_imp_print_2d_test():\n' );
  fprintf ( 1, '  circle_imp_print_2d() prints a circle definition.\n' );

  circle_imp_print_2d ( r, center, '  An example circle:' );

  return
end
