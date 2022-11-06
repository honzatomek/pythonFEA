function dist_num = dist_enum ( m, n )

%*****************************************************************************80
%
%% dist_enum() returns the number of distributions of indistinguishable objects.
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
%  Input:
%
%    integer M, the number of distinguishable "slots".
%
%    integer N, the number of indistinguishable objects.
%
%  Output:
%
%    integer DIST_NUM, the number of distributions of N
%    indistinguishable objects about M distinguishable slots.
%
  dist_num = i4_choose ( m + n - 1, n );

  return
end
