function i4vec_print_test ( )

%*****************************************************************************80
%
%% i4vec_print_test() tests i4vec_print().
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    01 November 2014
%
%  Author:
%
%    John Burkardt
%
  n = 4;
  a = [ 91, 92, 93, 94 ];

  fprintf ( 1, '\n' );
  fprintf ( 1, 'i4vec_print_test():\n' );
  fprintf ( 1, '  i4vec_print() prints an I4VEC\n' );

  i4vec_print ( n, a, '  The I4VEC:' );

  return
end
