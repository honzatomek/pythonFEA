function max_index_last = i4vec_max_index_last ( n, a )

%*****************************************************************************80
%
%% i4vec_max_index() returns the index of the last largest entry in an I4VEC.
%
%  Discussion:
%
%    An I4VEC is a vector of integer values.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license. 
%
%  Modified:
%
%    05 November 2005
%
%  Author:
%
%    John Burkardt
%
%  Input:
%
%    integer N, the number of entries in the vector.
%
%    integer A(N), the vector to be searched.
%
%  Output:
%
%    integer MAX_INDEX_LAST, the index of the last largest entry.
%
  if ( n <= 0 )

    max_index_last = 0;

  else

    amax = a(1);
    max_index_last = 1;

    for i = 2 : n

      if ( amax <= a(i) )
        amax = a(i);
        max_index_last = i;
      end

    end

  end

  return
end
