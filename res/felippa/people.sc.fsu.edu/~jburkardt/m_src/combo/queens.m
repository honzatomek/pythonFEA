function [ nstack, istack ] = queens ( n, iarray, k, nstack, istack, maxstack )

%*****************************************************************************80
%
%% queens() finds possible positions for the K-th nonattacking queen.
%
%  Discussion:
%
%    The chessboard is N by N, and is being filled one column at a time,
%    with a tentative solution to the nonattacking queen problem.  So
%    far, K-1 rows have been chosen, and we now need to provide a list
%    of all possible rows that might be used in column K.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    23 January 2011
%
%  Author:
%
%    John Burkardt
%
%  Input:
%
%    integer N, the total number of queens to place, and
%    the length of a side of the chessboard.
%
%    integer IARRAY(N).  The first K-1 entries of IARRAY
%    record the rows into which queens have already been placed.
%
%    integer K, the column for which we need possible
%    row positions for the next queen.
%
%    integer NSTACK, the current length of stack.
%
%    integer ISTACK(MAXSTACK), a list of the candidates, and
%    the number of candidates.
%
%    integer MAXSTACK, maximum dimension of ISTACK.
%
%  Output:
%
%    integer NSTACK, the updated length of stack.
%
%    integer ISTACK(MAXSTACK).  On output, we have added
%    the candidates, and the number of candidates, to the end of the
%    stack.
%
  ncan = 0;

  for irow = 1 : n
%
%  If row IROW has already been used, that is it.
%
    row = 0;

    for jcol = 1 : k - 1
      if ( iarray(jcol) == irow )
        row = 1;
      end
    end

    if ( ~row )

      diag = 0;

      for jcol = 1 : k - 1

        if ( irow == iarray(jcol) + k - jcol | ...
             irow == iarray(jcol) - ( k - jcol ) )

          diag = 1;

        end

      end

      if ( ~diag )
        ncan = ncan + 1;
        nstack = nstack + 1;
        istack(nstack) = irow;
      end

    end

  end

  nstack = nstack + 1;
  istack(nstack) = ncan;

  return
end
