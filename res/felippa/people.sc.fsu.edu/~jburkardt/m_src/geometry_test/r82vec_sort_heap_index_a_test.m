function r82vec_sort_heap_index_a_test ( )

%*****************************************************************************80
%
%% r82vec_sort_heap_index_a_test() tests r82vec_sort_heap_index_a().
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    19 April 2009
%
%  Author:
%
%    John Burkardt
%
  n = 12;
  b = 0.0;
  c = 10.0;

  fprintf ( 1, '\n' );
  fprintf ( 1, 'r82vec_sort_heap_index_a_test():\n' );
  fprintf ( 1, '  r82vec_sort_heap_index_a() index sorts an R82VEC\n' );
  fprintf ( 1, '  using heapsort.\n' );

  a = 10.0 * rand ( 2, n );
%
%  Give a few elements the same first component.
%
  a(1,3) = a(1,5);
  a(1,4) = a(1,12);
%
%  Give a few elements the same second component.
%
  a(2,6) = a(2,1);
  a(2,2) = a(2,9);
%
%  Make two entries equal.
%
  a(1:2,7) = a(1:2,11);

  r82vec_print ( n, a, '  Before rearrangement:' )

  indx = r82vec_sort_heap_index_a ( n, a );

  fprintf ( 1, '\n' );
  fprintf ( 1, '     I Index A(Index)\n' );
  fprintf ( 1, '\n' );

  for i = 1 : n
    fprintf ( 1, '  %6d  %6d  %6d\n', i, indx(i), a(1:2,indx(i)) );
  end

  a = r82vec_permute ( n, a, indx );

  r82vec_print ( n, a, '  After rearrangement by R82VEC_PERMUTE:' );

  return
end
