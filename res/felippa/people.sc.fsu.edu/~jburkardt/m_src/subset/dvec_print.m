function dvec_print ( n, dvec, title )

%*****************************************************************************80
%
%% dvec_print() prints a DVEC.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    26 May 2015
%
%  Author:
%
%    John Burkardt
%
%  Input:
%
%    integer N, the dimension of the vector.
%
%    real DVEC(N), the vector to be printed.
%
%    string TITLE, a title to be printed first.
%    TITLE may be blank.
%
  if ( 0 < s_len_trim ( title ) )
    fprintf ( 1, '\n' );
    fprintf ( 1, '%s\n', title );
    fprintf ( 1, '\n' );
  end

  if ( dvec(n) == 9 )
    fprintf ( 1, '-' );
  else
    fprintf ( 1, '+' );
  end

  for i = n - 1 : -1 : 1
    fprintf ( 1, '%d', dvec(i) );
  end
  fprintf ( 1, '\n' );

  return
end
