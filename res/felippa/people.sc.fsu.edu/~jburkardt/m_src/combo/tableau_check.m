function check = tableau_check ( n, tab )

%*****************************************************************************80
%
%% tableau_check() checks a 2 by N tableau.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    20 January 2011
%
%  Author:
%
%    John Burkardt
%
%  Reference:
%
%    Donald Kreher, Douglas Simpson,
%    Combinatorial Algorithms,
%    CRC Press, 1998,
%    ISBN: 0-8493-3988-X,
%    LC: QA164.K73.
%
%  Input:
%
%    integer N, the number of columns in the tableau.
%    N must be positive.
%
%    integer TAB(2,N), a 2 by N tableau.
%
%  Output:
%
%    integer CHECK.
%    1, the data is legal.
%    0, the data is not legal.
%
  check = 1;

  if ( n < 1 )
    check = 0;
    return
  end
%
%  The entries must be between 1 and 2*N.
%
  for i = 1 : 2
    for j = 1 : n
      if ( tab(i,j) < 1 | 2 * n < tab(i,j) )
        check = 0;
        return
      end
    end
  end
%
%  The entries must be increasing to the right.
%
  for i = 1 : 2
    for j = 2 : n
      if ( tab(i,j) <= tab(i,j-1) )
        check = 0;
        return
      end
    end
  end
%
%  The entries must be increasing down.
%
  i = 2;
  for j = 1 : n
    if ( tab(i,j) <= tab(i-1,j) )
      check = 0;
      return
    end
  end

  return
end
