function i4mat_transpose_print ( m, n, a, title )

%*****************************************************************************80
%
%% i4mat_transpose_print() prints an I4MAT, transposed.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    31 January 2005
%
%  Author:
%
%    John Burkardt
%
%  Input:
%
%    integer M, N, the number of rows and columns.
%
%    integer A(M,N), an M by N matrix to be printed.
%
%    string TITLE, an optional title.
%
  i4mat_transpose_print_some ( m, n, a, 1, 1, m, n, title );

  return
end
