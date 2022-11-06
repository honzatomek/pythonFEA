function number = compnz_enum ( n, k )

%*****************************************************************************80
%
%% compnz_enum() returns the number of nonzero compositions of the N into K parts.
%
%  Discussion:
%
%    A composition of the integer N into K nonzero parts is an ordered sequence
%    of K positive integers which sum to N.  The compositions (1,2,1)
%    and (1,1,2) are considered to be distinct.
%
%    The 10 compositions of 6 into three nonzero parts are:
%
%      4 1 1,  3 2 1,  3 1 2,  2 3 1,  2 2 2,  2 1 3,
%      1 4 1,  1 3 2,  1 2 3,  1 1 4.
%
%    The formula for the number of compositions of N into K nonzero
%    parts is
%
%      Number = ( N - 1 )! / ( ( N - K )! * ( K - 1 )! )
%
%    (Describe the composition using N-K '1's and K-1 dividing lines '|'.
%    The number of distinct permutations of these symbols is the number
%    of compositions into nonzero parts.  This is equal to the number of
%    permutations of  N-1 things, with N-K identical of one kind
%    and K-1 identical of another.)
%
%    Thus, for the above example, we have:
%
%      Number = ( 6 - 1 )! / ( ( 6 - 3 )! * ( 3 - 1 )! ) = 10
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    13 January 2021
%
%  Author:
%
%    John Burkardt
%
%  Reference:
%
%    Albert Nijenhuis, Herbert Wilf,
%    Combinatorial Algorithms for Computers and Calculators,
%    Second Edition,
%    Academic Press, 1978,
%    ISBN: 0-12-519260-6,
%    LC: QA164.N54.
%
%  Input:
%
%    integer N, the integer whose compositions are desired.
%
%    integer K, the number of parts in the composition.
%
%  Output:
%
%    integer NUMBER, the number of compositions of N into
%    K nonzero parts.
%
  number = i4_choose ( n - 1, n - k );

  return
end
