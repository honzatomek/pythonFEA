function s2 = stirling2_number ( n, k )

%*****************************************************************************80
%
%% stirling2_number() computes a Stirling number S2(N,K) of the second kind.
%
%  Discussion:
%
%    S2(N,K) represents the number of distinct partitions of N elements
%    into K nonempty sets.  For a fixed N, the sum of the Stirling
%    numbers S2(N,K) is represented by B(N), called "Bell's number",
%    and represents the number of distinct partitions of N elements.
%
%    For example, with 4 objects, there are:
%
%    1 partition into 1 set:
%
%      (A,B,C,D)
%
%    7 partitions into 2 sets:
%
%      (A,B,C) (D)
%      (A,B,D) (C)
%      (A,C,D) (B)
%      (A) (B,C,D)
%      (A,B) (C,D)
%      (A,C) (B,D)
%      (A,D) (B,C)
%
%    6 partitions into 3 sets:
%
%      (A,B) (C) (D)
%      (A) (B,C) (D)
%      (A) (B) (C,D)
%      (A,C) (B) (D)
%      (A,D) (B) (C)
%      (A) (B,D) (C)
%
%    1 partition into 4 sets:
%
%      (A) (B) (C) (D)
%
%    So S2(4,1) = 1, S2(4,2) = 7, S2(4,3) = 6, S2(4,4) = 1, and B(4) = 15.
%
%  First terms:
%
%    N/K: 1    2    3    4    5    6    7    8
%
%    1    1    0    0    0    0    0    0    0
%    2    1    1    0    0    0    0    0    0
%    3    1    3    1    0    0    0    0    0
%    4    1    7    6    1    0    0    0    0
%    5    1   15   25   10    1    0    0    0
%    6    1   31   90   65   15    1    0    0
%    7    1   63  301  350  140   21    1    0
%    8    1  127  966 1701 1050  266   28    1
%
%  Recursion:
%
%    S2(N,1) = 1 for all N.
%    S2(I,I) = 1 for all I.
%    S2(I,J) = 0 if I < J.
%
%    S2(N,K) = K * S2(N-1,K) + S2(N-1,K-1)
%
%  Direct Formula:
%
%    s2(n,k) = 1/k! sum ( 0 <= i <= k ) (-1)^i k-choose-i ( k - i )^n
%
%  Properties:
%
%    sum ( 1 <= K <= M ) S2(I,K) * S1(K,J) = Delta(I,J)
%
%    X^N = sum ( 0 <= K <= N ) S2(N,K) X_K
%    where X_K is the falling factorial function.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    15 July 2022
%
%  Author:
%
%    John Burkardt
%
%  Input:
%
%    integer N, the number of rows of the table.
%
%    integer K, the number of columns of the table.
%
%  Output:
%
%    integer S2, the Stirling number of the second kind.
%
  s2 = 0;
  for i = 0 : k
    s2 = s2 + i4_mop ( i ) * nchoosek ( k, i ) * ( k - i ) ^ n;
  end

  s2 = s2 / factorial ( k );

  return
end

