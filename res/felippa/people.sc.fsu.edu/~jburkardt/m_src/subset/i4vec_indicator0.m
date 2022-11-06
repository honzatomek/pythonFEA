function  a = i4vec_indicator0 ( n )

%*****************************************************************************80
%
%% i4vec_indicator0() sets an I4VEC to the indicator vector (0,1,2,...).
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license. 
%
%  Modified:
%
%    27 September 2014
%
%  Author:
%
%    John Burkardt
%
%  Input:
%
%    integer N, the number of entries in the vector.
%
%  Output:
%
%    integer A(N), the vector.
%
  a = ( 0 : n - 1 );

  return
end
