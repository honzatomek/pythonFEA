function [ a, subsize, more, more2, m, m2 ] = subset_by_size_next ( n, a, ...
  subsize, more, more2, m, m2 )

%*****************************************************************************80
%
%% subset_by_size_next() returns all subsets of an N set, in order of size.
%
%  Example:
%
%    N = 4:
%
%    1 2 3 4
%    1 2 3
%    1 2 4
%    1 3 4
%    1 3
%    1 4
%    2 3
%    1
%    2
%    3
%    (the empty set)
%
%  Discussion:
%
%    The subsets are returned in decreasing order of size, with the
%    empty set last.
%
%    For a given size K, the K subsets are returned in lexicographic order.
%
%    On the first call, it is only important that MORE be set FALSE.  The
%    input values of A and SUBSIZE are not important.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license. 
%
%  Modified:
%
%    09 June 2015
%
%  Author:
%
%    John Burkardt
%
%  Input:
%
%    integer N, the size of the set.
%
%    integer A(N), the previous output subset.
%
%    integer SUBSIZE, the size of the previous output subset.
%
%    logical MORE, is FALSE on the first call, which signals
%    the routine to initialize itself.  Thereafter, MORE should be TRUE.
%
%    logical MORE2, a variable for bookkeeping.
%    The user should declare this variable, but need not initialize it.
%    The output value from one call must be the input value for the next.
%
%    integer M, M2, variables for bookkeeping.
%    The user should declare this variable, but need not initialize it.
%    The output value from one call must be the input value for the next.
%
%  Output:
%
%    integer A(N), the next subset.
%
%    integer SUBSIZE, the size of the next subset.
%
%    logical MORE, is TRUE as long as there are even more subsets
%    that can be produced by further calls.
%
%    logical MORE2, a variable for bookkeeping.
%    The user should declare this variable, but need not initialize it.
%    The output value from one call must be the input value for the next.
%
%    integer M, M2, variables for bookkeeping.
%    The user should declare this variable, but need not initialize it.
%    The output value from one call must be the input value for the next.
%
  if ( ~ more )
    subsize = n;
    more = true;
    more2 = false;
    m = 0;
    m2 = 0;
  else
    if ( ~ more2 )
      subsize = subsize - 1;
    end
  end
%
%  Compute the next subset of size SIZE.
%
  if ( 0 < subsize )
    [ a, more2, m, m2 ] = ksub_next ( n, subsize, a, more2, m, m2 );
  else
    more = false;
  end

  return
end
