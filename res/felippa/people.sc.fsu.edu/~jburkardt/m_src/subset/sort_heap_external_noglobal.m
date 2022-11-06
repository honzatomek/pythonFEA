function [ indx, i, j, i1, j1, k0, k1, n1 ] = sort_heap_external_noglobal ( ...
  n, indx, isgn, i1, j1, k0, k1, n1 )

%*****************************************************************************80
%
%% sort_heap_external_noglobal() externally sorts a list of items into ascending order.
%
%  Discussion:
%
%    The actual list of data is not passed to the routine.  Hence this
%    routine may be used to sort integers, reals, numbers, names,
%    dates, shoe sizes, and so on.  After each call, the routine asks
%    the user to compare or interchange two items, until a special
%    return value signals that the sorting is completed.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    15 June 2015
%
%  Author:
%
%    Original FORTRAN77 version by Albert Nijenhuis, Herbert Wilf.
%    MATLAB version by John Burkardt
%
%  Reference:
%
%    Albert Nijenhuis, Herbert Wilf.
%    Combinatorial Algorithms,
%    Academic Press, 1978, second edition,
%    ISBN 0-12-519260-6.
%
%  Input:
%
%    integer N, the number of items to be sorted.
%
%    integer INDX, the main communication signal.
%    The user must set INDX to 0 before the first call.
%    Thereafter, the user should set the input value of INDX
%    to the output value from the previous call.
%
%    integer ISGN, results of comparison of elements I and J.
%    (Used only when the previous call returned INDX less than 0).
%    ISGN <= 0 means I is less than or equal to J;
%    0 <= ISGN means I is greater than or equal to J.
%
%    integer I1, J1, K0, K1, N1, variables that
%    are used for bookkeeping.  The user should declare them, and pass the
%    output values from one call as input values on the next call.  The user
%    should not change these variables.
%
%  Output:
%
%    integer INDX, the main communication signal.
%    If INDX is
%      greater than 0, the user should:
%      * interchange items I and J;
%      * call again.
%      less than 0, the user should:
%      * compare items I and J;
%      * set ISGN = -1 if I < J, ISGN = +1 if J < I;
%      * call again.
%      equal to 0, the sorting is done.
%
%    integer I, J, the indices of two items.
%    On return with INDX positive, elements I and J should be interchanged.
%    On return with INDX negative, elements I and J should be compared, and
%    the result reported in ISGN on the next call.
%
%    integer I1, J1, K0, K1, N1, variables that
%    are used for bookkeeping.  The user should declare them, and pass the
%    output values from one call as input values on the next call.  The user
%    should not change these variables.
%

%
%  INDX = 0: This is the first call.
%
  if ( indx == 0 )
      
    k0 = floor ( n / 2 );
    k1 = floor ( n / 2 );
    n1 = n;
%
%  INDX < 0: The user is returning the results of a comparison.
%
  elseif ( indx < 0 )

    if ( indx == -2 )

      if ( isgn < 0 )
        i1 = i1 + 1;
      end

      j1 = k1;
      k1 = i1;
      indx = -1;
      i = i1;
      j = j1;
      return;
    end

    if ( 0 < isgn )
      indx = 2;
      i = i1;
      j = j1;
      return;
    end

    if ( k0 <= 1 )

      if ( n1 == 1 )
        i1 = 0;
        j1 = 0;
        indx = 0;
      else
        i1 = n1;
        n1 = n1 - 1;
        j1 = 1;
        indx = 1;
      end

      i = i1;
      j = j1;
      return;

    end

    k0 = k0 - 1;
    k1 = k0;
%
%  0 < INDX, the user was asked to make an interchange.
%
  elseif ( indx == 1 )

    k1 = k0;

  end

  while ( true )

    i1 = 2 * k1;

    if ( i1 == n1 )
      j1 = k1;
      k1 = i1;
      indx = -1;
      i = i1;
      j = j1;
      return;
    elseif ( i1 < n1 )
      j1 = i1 + 1;
      indx = -2;
      i = i1;
      j = j1;
      return;
    end

    if ( k0 <= 1 )
      break;
    end

    k0 = k0 - 1;
    k1 = k0;

  end

  if ( n1 == 1 )
    i1 = 0;
    j1 = 0;
    indx = 0;
    i = i1;
    j = j1;
  else
    i1 = n1;
    n1 = n1 - 1;
    j1 = 1;
    indx = 1;
    i = i1;
    j = j1;
  end

  return
end
