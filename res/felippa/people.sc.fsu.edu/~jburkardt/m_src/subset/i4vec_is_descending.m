function value = i4vec_is_descending ( n, x )

%*****************************************************************************80
%
%% i4vec_is_descending() is true if an I4VEC is decreasing.
%
%  Example:
%
%    X = ( 9, 7, 7, 3, 2, 1, -8 )
%
%    I4VEC_IS_DESCENDING = true
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license. 
%
%  Modified:
%
%    24 June 2016
%
%  Author:
%
%    John Burkardt
%
%  Input:
%
%    integer N, the size of the array.
%
%    integer X(N), the array to be examined.
%
%  Output:
%
%    logical VALUE, is true if the entries of X descend.
%
  value = true;

  for i = 1 : n - 1
    if ( x(i) < x(i+1) )
      value = false;
      break;
    end
  end

  return
end
