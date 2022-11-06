function [ n_data, n, c ] = mertens_values ( n_data )

%*****************************************************************************80
%
%% mertens_values() returns some values of the Mertens function.
%
%  Discussion:
%
%    The Mertens function M(N) is the sum from 1 to N of the Moebius
%    function MU.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    17 October 2007
%
%  Author:
%
%    John Burkardt
%
%  Reference:
%
%    M Deleglise, J Rivat,
%    Computing the Summation of the Moebius Function,
%    Experimental Mathematics,
%    Volume 5, 1996, pages 291-295.
%
%    Eric Weisstein,
%    CRC Concise Encyclopedia of Mathematics,
%    CRC Press, 2002,
%    Second edition,
%    ISBN: 1584883472,
%    LC: QA5.W45.
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
%    integer N_DATA, the index of the test data.
%
%    integer N, the argument of the Mertens function.
%
%    integer C, the value of the Mertens function.
%
  n_max = 15;
  c_vec = [ ...
      1,   0,  -1,   -1,  -2,  -1,  -2,  -2,   -2,  -1, ...
     -2,  -2,   1,    2, -23 ];
  n_vec = [ ...
      1,   2,   3,   4,   5,   6,   7,   8,   9,  10, ...
     11,  12,  100, 1000, 10000 ];

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
