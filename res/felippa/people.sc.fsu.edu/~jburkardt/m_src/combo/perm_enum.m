function nperm = perm_enum ( n )

%*****************************************************************************80
%
%% perm_enum() enumerates the permutations on N digits.
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
%    integer N, the number of values being permuted.
%    N must be nonnegative.
%
%  Output:
%
%    integer NPERM, the number of distinct elements.
%
  nperm = factorial ( n );

  return
end
