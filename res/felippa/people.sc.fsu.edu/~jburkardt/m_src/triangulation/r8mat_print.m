function r8mat_print ( m, n, a, title )

%*****************************************************************************80
%
%% r8mat_print() prints an R8MAT.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    06 September 2004
%
%  Author:
%
%    John Burkardt
%
%  Input:
%
%    integer M, the number of rows in A.
%
%    integer N, the number of columns in A.
%
%    real A(M,N), the matrix.
%
%    string TITLE, a title to be printed.
%
  r8mat_print_some ( m, n, a, 1, 1, m, n, title );

  return
end

