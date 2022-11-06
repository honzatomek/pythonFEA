function [ n_data, n, c ] = partition_distinct_count_values ( n_data )

%*****************************************************************************80
%
%% partition_distinct_count_values() returns some values of Q(N).
%
%  Discussion:
%
%    A partition of an integer N is a representation of the integer
%    as the sum of nonzero positive integers.  The order of the summands
%    does not matter.  The number of partitions of N is symbolized
%    by P(N).  Thus, the number 5 has P(N) = 7, because it has the
%    following partitions:
%
%    5 = 5
%      = 4 + 1
%      = 3 + 2
%      = 3 + 1 + 1
%      = 2 + 2 + 1
%      = 2 + 1 + 1 + 1
%      = 1 + 1 + 1 + 1 + 1
%
%    However, if we require that each member of the partition
%    be distinct, so that no nonzero summand occurs more than once,
%    we are computing something symbolized by Q(N).
%    The number 5 has Q(N) = 3, because it has the following partitions
%    into distinct parts:
%
%    5 = 5
%      = 4 + 1
%      = 3 + 2
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    26 May 2004
%
%  Author:
%
%    John Burkardt
%
%  Reference:
%
%    Milton Abramowitz and Irene Stegun,
%    Handbook of Mathematical Functions,
%    US Department of Commerce, 1964.
%
%  Input:
%
%    integer N_DATA.  The user sets N_DATA to 0 before the first call.
%
%  Output:
%
%    integer N_DATA.  On each call, the routine increments N_DATA by 1, and
%    returns the corresponding data; when there is no more data, the
%    output value of N_DATA will be 0 again.
%
%    integer N, the integer.
%
%    integer C, the number of partitions of the integer
%    into distinct parts.
%
  n_max = 21;
  c_vec = [ ...
      1, ...
      1,   1,   2,   2,   3,   4,   5,   6,   8,  10, ...
     12,  15,  18,  22,  27,  32,  38,  46,  54,  64 ];
  n_vec = [ ...
     0,  ...
     1,  2,  3,  4,  5,  6,  7,  8,  9, 10, ...
    11, 12, 13, 14, 15, 16, 17, 18, 19, 20 ];

  if ( n_data < 0 )
    n_data = 0;
  end

  n_data = n_data + 1;

  if ( n_max < n_data )
    n_data = 0;
    n = 0;
    c = 0;
  else
    n = n_vec(n_data);
    c = c_vec(n_data);
  end

  return
end
