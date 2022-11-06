function i4vec_heap_d_test ( )

%*****************************************************************************80
%
%% i4vec_heap_d_test() tests i4vec_heap_d().
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    14 April 2009
%
%  Author:
%
%    John Burkardt
%
  n = 10;
  b = 0;
  c = n;

  fprintf ( 1, '\n' );
  fprintf ( 1, 'i4vec_heap_d_test():\n' );
  fprintf ( 1, '  i4vec_heap_d() puts into descending heap form.\n' );

  a = randi ( [ 0, n ], n );

  i4vec_print ( n, a, '  Unsorted array:' );
 
  a = i4vec_heap_d ( n, a );
 
  i4vec_print ( n, a, '  Descending heap form:' );

  return
end
