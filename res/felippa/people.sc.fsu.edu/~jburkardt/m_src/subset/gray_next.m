function [ change, k, a ] = gray_next ( n, change, k, a )

%*****************************************************************************80
%
%% gray_next() generates the next Gray code by switching one item at a time.
%
%  Discussion:
%
%    On the first call only, the user must set CHANGE = -N.
%    This initializes the routine to the Gray code for N zeroes.
%
%    Each time it is called thereafter, it returns in CHANGE the index
%    of the item to be switched in the Gray code.  The sign of CHANGE
%    indicates whether the item is to be added or subtracted (or
%    whether the corresponding bit should become 1 or 0).  When
%    CHANGE is equal to N+1 on output, all the Gray codes have been
%    generated.
%
%  Example:
%
%    N  CHANGE         Subset in/out   Binary Number
%                      Interpretation  Interpretation
%                       1 2 4 8
%   --  ---------      --------------  --------------
%
%    4   -4 / 0         0 0 0 0         0
%
%        +1             1 0 0 0         1
%          +2           1 1 0 0         3
%        -1             0 1 0 0         2
%            +3         0 1 1 0         6
%        +1             1 1 1 0         7
%          -2           1 0 1 0         5
%        -1             0 0 1 0         4
%              +4       0 0 1 1        12
%        +1             1 0 1 1        13
%          +2           1 1 1 1        15
%        -1             0 1 1 1        14
%            -3         0 1 0 1        10
%        +1             1 1 0 1        11
%          -2           1 0 0 1         9
%        -1             0 0 0 1         8
%              -4       0 0 0 0         0
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    12 June 2015
%
%  Author:
%
%    John Burkardt
%
%  Reference:
%
%    Albert Nijenhuis, Herbert Wilf,
%    Combinatorial Algorithms,
%    Academic Press, 1978, second edition,
%    ISBN 0-12-519260-6.
%
%  Input:
%
%    integer N, the order of the total set from which
%    subsets will be drawn.
%
%    integer CHANGE.  This is an input item only
%    on the first call for a particular sequence of Gray codes,
%    at which time it must be set to -N.  This corresponds to
%    all items being excluded, or all bits being 0, in the Gray code.
%
%    integer K, a bookkeeping variable.
%    The user must declare this variable before the first call.
%    The output value from one call should be the input value for the next.
%    The user should not change this variable.
%
%    integer A(N), a bookkeeping variable.
%    The user must declare this variable before the first call.
%    The output value from one call should be the input value for the next.
%    The user should not change this variable.
%
%  Output:
%
%    integer CHANGE, indicates which of the N items must be
%    "changed", and the sign indicates whether the item is to be added
%    or removed (or the bit is to become 1 or 0).  Note that on return
%    from the first call only, CHANGE has the value 0, indicating
%    that the first set is the empty set, or the number 0.
%
%    integer K, a bookkeeping variable.
%    The user should not change this variable.
%
%    integer A(N), a bookkeeping variable.
%    The user should not change this variable.
%
  if ( n <= 0 )
    fprintf ( 1, '\n' );
    fprintf ( 1, 'GRAY_NEXT - Fatal error!\n' );
    fprintf ( 1, '  Input value of N <= 0.\n' );
    error ( 'GRAY_NEXT - Fatal error!' );
  end

  if ( change == -n )
    change = 0;
    k = 1;
    a(1:n) = 0;
    return
  end
%
%  First determine WHICH item is to be changed.
%
  if ( mod ( k, 2 ) == 1 )

    change = 1;

  else

    for i = 1 : n
      if ( a(i) == 1 )
        change = i + 1;
        break
      end
    end

  end
%
%  Take care of the terminal case CHANGE = N + 1.
%
  if ( change == n + 1 )
    change = n;
  end
%
%  Now determine HOW the item is to be changed.
%
  if ( a(change) == 0 )
    a(change) = 1;
  elseif ( a(change) == 1 )
    a(change) = 0;
    change = -change;
  end
%
%  Update the counter.
%
  k = k + 1;
%
%  If the output CHANGE = -N, then we're done.
%
  if ( change == -n )
    k = 0;
  end

  return
end
 
