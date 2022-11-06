function i4vec_print ( n, a, title )

%*****************************************************************************80
%
%% i4vec_print() prints an I4VEC.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license. 
%
%  Modified:
%
%    25 January 2004
%
%  Author:
%
%    John Burkardt
%
%  Input:
%
%    integer N, the dimension of the vector.
%
%    integer A(N), the vector to be printed.
%
%    string TITLE, a title to be printed first.
%    TITLE may be blank.
%
  if ( 0 < s_len_trim ( title ) )
    fprintf ( 1, '\n' );
    fprintf ( 1, '%s\n', title );
  end

  fprintf ( 1, '\n' );
  for i = 1 : n
    fprintf ( 1, '%6d  %6d\n', i, a(i) );
  end

  return
end
