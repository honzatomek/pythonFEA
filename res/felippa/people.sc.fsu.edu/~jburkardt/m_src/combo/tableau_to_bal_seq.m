function t = tableau_to_bal_seq ( n, tab )

%*****************************************************************************80
%
%% tableau_to_bal_seq() converts a 2 by N tableau to a balanced sequence.
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
%    integer N, the number of 0's (and 1's) in the sequence.
%    N must be positive.
%
%    integer TAB(2,N), a 2 by N tableau.
%
%  Output:
%
%    integer T(2*N), a balanced sequence.
%

%
%  Check.
%
  check = tableau_check ( n, tab );

  if ( ~ check )
    fprintf ( 1, '\n' );
    fprintf ( 1, 'TABLEAU_TO_BAL_SEQ - Fatal error!\n' );
    fprintf ( 1, '  The input array is illegal.\n' );
    error ( 'TABLEAU_TO_BAL_SEQ - Fatal error!' );
  end

  for i = 1 : 2
    for j = 1 : n
      t(tab(i,j)) = i - 1;
    end
  end

  return
end
