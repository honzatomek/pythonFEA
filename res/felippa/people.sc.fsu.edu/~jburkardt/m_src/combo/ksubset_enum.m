function nksub = ksubset_enum ( k, n )

%*****************************************************************************80
%
%% ksubset_enum() enumerates the K element subsets of an N set.
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
%    integer K, the number of elements each K subset must
%    have. 0 <= K <= N.
%
%    integer N, the number of elements in the master set.
%    0 <= N.
%
%  Output:
%
%    integer NKSUB, the number of distinct elements.
%
  nksub = nchoosek ( n, k );

  return
end
