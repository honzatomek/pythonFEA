function a = i4col_sort2_a ( m, n, a )

%*****************************************************************************80
%
%% i4col_sort2_a() ascending sorts the elements of each column of an I4COL.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    28 October 2005
%
%  Author:
%
%    John Burkardt
%
%  Input:
%
%    integer M, the number of rows of A.
%
%    integer N, the number of columns of A, and the length
%    of a vector of data.
%
%    integer A(M,N), the array of N columns of M vectors.
%
%  Output:
%
%    integer A(M,N), the elements of each column of A have been 
%    sorted in ascending order.
%
  if ( m <= 1 )
    return
  end

  if ( n <= 0 )
    return
  end
%
%  Initialize.
%
  for col = 1 : n

    i = 0;
    indx = 0;
    isgn = 0;
    j = 0;
%
%  Call the external heap sorter.
%
    while ( true )

      [ indx, i, j ] = sort_heap_external ( m, indx, isgn );
%
%  Interchange the I and J objects.
%
      if ( 0 < indx )

        t        = a(i,col);
        a(i,col) = a(j,col);
        a(j,col) = t;
%
%  Compare the I and J objects.
%
      elseif ( indx < 0 )

        if ( a(j,col) < a(i,col) )
          isgn = +1;
        else
          isgn = -1;
        end

      elseif ( indx == 0 )

        break

      end

    end

  end

  return
end
