function r82vec_print_test ( )

%*****************************************************************************80
%
%% r82vec_print_test() tests r82vec_print().
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    01 August 2018
%
%  Author:
%
%    John Burkardt
%
  n = 4;
  x = -10.0 + 20.0 * rand ( 2, n );

  fprintf ( 1, '\n' );
  fprintf ( 1, 'r82vec_print_test():\n' );
  fprintf ( 1, '  r82vec_print() prints an R82VEC.\n' );

  r82vec_print ( n, x, '  The R82VEC:' );

  return
end
