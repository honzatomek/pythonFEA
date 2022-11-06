function rank = perm_lex_rank ( n, p )

%*****************************************************************************80
%
%% perm_lex_rank() computes the lexicographic rank of a permutation.
%
%  Discussion:
%
%    The original code altered the input permutation.  
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license. 
%
%  Modified:
%
%    11 January 2021
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
%    integer N, the number of values being permuted.
%    N must be positive.
%
%    integer P(N), describes the permutation.
%    P(I) is the item which is permuted into the I-th place
%    by the permutation.
%
%  Output:
%
%    integer RANK, the rank of the permutation.
%

%
%  Check.
%
  check = perm_check ( n, p );

  if ( ~ check )
    fprintf ( 1, '\n' );
    fprintf ( 1, 'perm_lex_rank(): Fatal error!\n' );
    fprintf ( 1, '  The input array is illegal.\n' );
    error ( 'perm_lex_rank(): Fatal error!' );
  end

  p2(1:n) = p(1:n);

  rank = 0;

  for j = 1 : n

    rank = rank + ( p2(j) - 1 ) * factorial ( n - j );

    for i = j + 1 : n
      if ( p2(j) < p2(i) )
        p2(i) = p2(i) - 1;
      end
    end

  end

  return
end
