function value = i4vec_index ( n, a, aval )

%*****************************************************************************80
%
%% i4vec_index() returns the first location of a given value in an I4VEC
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
%    integer AVAL, the value to be indexed.
%
%  Output:
%
%    integer VALUE, the first location in A which has the
%    value AVAL, or -1 if no such index exists.
%
  for i = 1 : n
    if ( a(i) == aval )
      value = i;
      return
    end
  end

  value = -1;

  return
end
