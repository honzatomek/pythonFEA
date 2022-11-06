function tab = bal_seq_to_tableau ( n, t )

%*****************************************************************************80
%
%% bal_seq_to_tableau() converts a balanced sequence to a 2 by N tableau.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    22 January 2011
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
%    integer T(2*N), a balanced sequence.
%
%  Output:
%
%    integer TAB(2,N), a 2 by N tableau.
%

%
%  Check.
%
  check = bal_seq_check ( n, t );

  if ( ~ check )
    fprintf ( 1, '\n' );
    fprintf ( 1, 'BAL_SEQ_TO_TABLEAU - Fatal error!\n' );
    fprintf ( 1, '  The input array is illegal.\n' );
    error ( 'BAL_SEQ_TO_TABLEAU - Fatal error!' );
  end

  c(1) = 0;
  c(2) = 0;
  for i = 1 : 2 * n
    r = t(i) + 1;
    c(r) = c(r) + 1;
    tab(r,c(r)) = i;
  end

  return
end
