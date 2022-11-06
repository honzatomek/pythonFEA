function value = pyramid_num ( n )

%*****************************************************************************80
%
%% pyramid_num() returns the N-th pyramidal number.
%
%  Discussion:
%
%    The N-th pyramidal number P(N) is formed by the sum of the first
%    N triangular numbers T(J):
%
%      T(J) = sum ( 1 <= J <= N ) J
%
%      P(N) = sum ( 1 <= I <= N ) T(I)
%
%    By convention, T(0) = 0.
%
%    P(N) = ( (N+1)^3 - (N+1) ) / 6
%
%    Note that this pyramid will have a triangular base.
%
%    The first values are:
%
%      0
%      1
%      4
%     10
%     20
%     35
%     56
%     84
%    120
%    165
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    25 July 2004
%
%  Author:
%
%    John Burkardt
%
%  Input:
%
%    integer N, the index of the desired number, which must be
%    at least 0.
%
%  Output:
%
%    integer VALUE, the N-th pyramidal number.
%
  value = ( ( n + 1 )^3 - ( n + 1 ) ) / 6;

  return
end
