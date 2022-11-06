function i4vec_print ( n, a, label )

%*****************************************************************************80
%
%% I4VEC_PRINT prints an I4VEC.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license. 
%
%  Modified:
%
%    21 July 2018
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
%    string LABEL, a title.
%
  if ( 0 < length ( label ) )
    fprintf ( 1, '\n' );
    fprintf ( 1, '%s\n', label );
  end
  fprintf ( 1, '\n' );

  for i = 1 : n
    fprintf ( 1, '%6d: %6d\n', i, a(i) );
  end

  return
end
