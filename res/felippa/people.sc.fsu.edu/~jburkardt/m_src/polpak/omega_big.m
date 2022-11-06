function ndiv = omega_big ( n )

%*****************************************************************************80
%
%% omega_big() returns big omega(N), the number of prime divisors of N.
%
%  Discussion:
%
%    The big omega function counts the number of divisors in the prime
%    factorization of N, and allows multiplicity.
%
%  Note that litte omega counts the number of distinct factors,
%  while big omega counts a multiple factor multiple times:
%
%     N   omega_little(N)  omega_big(N)
%
%     1    1   1
%     2    1   1
%     3    1   1
%     4    1   2
%     5    1   1
%     6    2   2
%     7    1   1
%     8    1   3
%     9    1   2
%    10    2   2
%    11    1   1
%    12    2   3
%    13    1   1
%    14    2   2
%    15    2   2
%    16    1   4
%    17    1   1
%    18    2   3
%    19    1   1
%    20    2   3
%
%  Formula:
%
%    If N = 1, then
%
%      omega_big(N) = 1
%
%    else if the prime factorization of N is
%
%      N = P1^E1 * P2^E2 * ... * PM^EM,
%
%    then
%
%      omega_big(N) = E1 + E2 + ... + EM.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    05 January 2021
%
%  Author:
%
%    John Burkardt
%
%  Input:
%
%    integer N, the value to be analyzed.  N must be 1 or greater.
%
%  Output:
%
%    integer NDIV, the value of omega_big(N).  But if N is 0 or
%    less, NDIV is returned as 0, a nonsense value.  If there is
%    not enough room for factoring, NDIV is returned as -1.
%
  if ( n <= 0 )
    ndiv = 0;
    return
  end

  if ( n == 1 )
    ndiv = 1;
    return
  end
%
%  Factor N.
%
  [ nfactor, factor, power, nleft ] = i4_factor ( n );

  if ( nleft ~= 1 )
    fprintf ( 1, '\n' );
    fprintf ( 1, 'omega_big - Fatal error!\n' );
    fprintf ( 1, '  Not enough factorization space.\n' );
    error ( 'omega_big - Fatal error!' );
  end

  ndiv = sum ( power(1:nfactor) );

  return
end
