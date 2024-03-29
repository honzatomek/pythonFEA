function  a = i4vec_indicator ( n )

%*****************************************************************************80
%
%% i4vec_indicator() sets an I4VEC to the indicator vector.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license. 
%
%  Modified:
%
%    18 November 2003
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
%    integer A(N), the vector with entries (1, 2, ..., N ).
%
  a = ( 1 : n );

  return
end
