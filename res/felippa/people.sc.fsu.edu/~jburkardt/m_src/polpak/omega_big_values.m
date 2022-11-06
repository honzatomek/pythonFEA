function [ n_data, n, c ] = omega_big_values ( n_data )

%*****************************************************************************80
%
%% omega_big_values() returns some values of the big omega function.
%
%  First values:
%
%     N   omega_big(N)
%
%     1    1
%     2    1
%     3    1
%     4    2
%     5    1
%     6    2
%     7    1
%     8    3
%     9    2
%    10    2
%    11    1
%    12    3
%    13    1
%    14    2
%    15    2
%    16    4
%    17    1
%    18    3
%    19    1
%    20    3
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
%  Reference:
%
%    Milton Abramowitz, Irene Stegun,
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
%    integer N, the argument.
%
%    integer C, the value.
%
  n_max = 23;
  c_vec = [ ...
      1,   1,   1,   2,   1, ...
      2,   1,   3,   2,   2, ...
      3,   1,   4,   6,   6, ...
      1,   5,   2,   2,   1, ...
      6,   7,   8 ];
  n_vec = [ ...
           1,        2,        3,        4,        5, ...
           6,        7,        8,        9,       10, ...
          30,      101,      210,     1320,     1764, ...
        2003,     2310,     2827,     8717,    12553, ...
       30030,   510510,  9699690 ];

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
