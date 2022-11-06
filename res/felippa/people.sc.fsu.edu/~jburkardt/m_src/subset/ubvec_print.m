function ubvec_print ( n, bvec, title )

%*****************************************************************************80
%
%% ubvec_print() prints a UBVEC, with an optional title.
%
%  Discussion:
%
%    A UBVEC is an integer vector of binary digits, intended to
%    represent a nonnegative integer.  BVEC(1) is the units digit, BVEC(N)
%    is the coefficient of 2^(N-1).
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    27 December 2014
%
%  Author:
%
%    John Burkardt
%
%  Input:
%
%    integer N, the number of components of the vector.
%
%    integer BVEC(N), the vector to be printed.
%
%    string TITLE, a title to be printed first.
%    TITLE may be blank.
%
  if ( 0 < s_len_trim ( title ) )
    fprintf ( 1, '\n' );
    fprintf ( 1, '%s\n', title );
  end

  for ihi = n : -70 : 1
    ilo = max ( ihi - 70 + 1, 1 );
    fprintf ( 1, '  ' );
    for i = ihi : -1 : ilo
      fprintf ( 1, '%1d', bvec(i) );
    end
    fprintf ( 1, '\n' );
  end

  return
end
