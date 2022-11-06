function sort_heap_external_test ( )

%*****************************************************************************80
%
%% sort_heap_external_test() tests sort_heap_external().
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    15 June 2015
%
%  Author:
%
%    John Burkardt
%
  n = 20;

  fprintf ( 1, '\n' );
  fprintf ( 1, 'sort_heap_external_test():\n' );
  fprintf ( 1, '  sort_heap_external() sorts objects externally.\n' );

  a = randi ( [ 1, n ], n );

  i4vec_print ( n, a, '  Unsorted array:' );

  indx = 0;
  isgn = 0;

  while ( true )

    [ indx, i, j ] = sort_heap_external ( n, indx, isgn );

    if ( indx < 0 )
      isgn = 1;
      if ( a(i) <= a(j) )
        isgn = -1;
      end 
    elseif ( 0 < indx )
      t = a(i);
      a(i) = a(j);
      a(j) = t;
    else
      break;
    end

  end

  i4vec_print ( n, a, '  Sorted array:' );

  return
end
