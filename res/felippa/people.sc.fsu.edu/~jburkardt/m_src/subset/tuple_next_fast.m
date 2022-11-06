function [ x, base ] = tuple_next_fast ( m, n, rank, base )

%*****************************************************************************80
%
%% tuple_next_fast() computes the next element of a tuple space, "fast".
%
%  Discussion:
%
%    The elements are N vectors.  Each entry is constrained to lie
%    between 1 and M.  The elements are produced one at a time.
%    The first element is
%      (1,1,...,1)
%    and the last element is
%      (M,M,...,M)
%    Intermediate elements are produced in lexicographic order.
%
%    This code was written as a possibly faster version of TUPLE_NEXT.
%
%  Example:
%
%    N = 2,
%    M = 3
%
%    INPUT        OUTPUT
%    -------      -------
%    Rank          X
%    ----          ----
%   -1            -1 -1
%
%    0             1  1
%    1             1  2
%    2             1  3
%    3             2  1
%    4             2  2
%    5             2  3
%    6             3  1
%    7             3  2
%    8             3  3
%    9             1  1
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    04 June 2015
%
%  Author:
%
%    John Burkardt
%
%  Input:
%
%    integer M, the maximum entry in each component.
%    M must be greater than 0.
%
%    integer N, the number of components.
%    N must be greater than 0.
%
%    integer RANK, indicates the rank of the tuples.
%    Typically, 0 <= RANK < N^M; values greater than this are
%    legal and meaningful, being equivalent to the corresponding
%    value mod N^M.  RANK < 0 indicates that this is the first
%    call for the given values of (M,N).  Initialization is done,
%    and X is set to a dummy value.
%
%    integer BASE(N), a bookkeeping array.
%    The user should allocate space for this array, but
%    should not alter it between successive calls.
%
%  Output:
%
%    integer X(N), the next tuple of the given rank,
%    or a dummy value if initialization is being done.
%
%    integer BASE(N), the updated bookkeeping array.
%
  if ( rank < 0 )

    if ( m <= 0 )
      fprintf ( 1, '\n' );
      fprintf ( 1, 'TUPLE_NEXT_FAST - Fatal error!\n' );
      fprintf ( 1, '  M <= 0 is illegal.\n' );
      fprintf ( 1, '  M = %d\n', m );
      error ( 'TUPLE_NEXT_FAST - Fatal error!' );
    end

    if ( n <= 0 )
      fprintf ( 1, '\n' );
      fprintf ( 1, 'TUPLE_NEXT_FAST - Fatal error!\n' );
      fprintf ( 1, '  N <= 0 is illegal.\n' );
      fprintf ( 1, '  N = %d\n', n );
      error ( 'TUPLE_NEXT_FAST - Fatal error!' );
    end

    base(n) = 1;
    for i = n - 1 : -1 : 1
     base(i) = base(i+1) * m;
    end

    x(1:n) = -1;

  else

    x(1:n) = mod ( floor ( rank ./ base(1:n) ), m ) + 1;

  end

  return
end
