function [ a, more, maxder, numder ] = derange1_weed_next ( n, a, more, ...
  maxder, numder )

%*****************************************************************************80
%
%% derange1_weed_next() computes derangements of (1,...,N), one at a time.
%
%  Discussion:
%
%    A derangement of N objects is a permutation which leaves no object
%    unchanged.
%
%    A derangement of N objects is a permutation with no fixed
%    points.  If we symbolize the permutation operation by "P",
%    then for a derangment, P(I) is never equal to I.
%
%    The number of derangements of N objects is sometimes called
%    the subfactorial function, or the derangement number D(N).
%
%    This routine simply generates all permutations, one at a time,
%    and weeds out those that are not derangements.
%
%  Example:
%
%    Here are the derangements when N = 4:
%
%    2143
%    2341
%    2413
%    3142
%    3412
%    3421
%    4123
%    4312
%    4321
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    08 June 2015
%
%  Author:
%
%    John Burkardt
%
%  Input:
%
%    integer N, the number of objects being permuted.
%
%    integer A(N).  On an initialization call, A is ignored.
%    Otherwise, A should be the output value of A from the previous call.
%
%    logical MORE, is FALSE on an initialization call, and TRUE otherwise.
%
%    integer MAXDER, NUMDER, two parameters
%    used by the program for bookkeeping.  The user should declare these
%    variables, and pass the output values from one call to the next,
%    but should not alter them.
%
%  Output:
%
%    integer A(N), if MORE is TRUE, the next derangement.
%    If MORE is FALSE, then A contains no useful information.
%
%    logical MORE is TRUE if the next derangement was output in
%    A, and FALSE if there are no more derangements.
%
%    integer MAXDER, NUMDER, two parameters
%    used by the program for bookkeeping.  The user should declare these
%    variables, and pass the output values from one call to the next,
%    but should not alter them.
%

%
%  Initialization on call with MORE = FALSE.
%
  if ( ~ more )
    a = [];
    maxder = derange_enum ( n );
    numder = 0;
  end
%
%  Watch out for cases where there are no derangements.
%
  if ( maxder == 0 )
    more = false;
    return
  end
%
%  Get the next permutation.
%
  while ( true )

    [ a, more ] = perm1_lex_next ( n, a, more );
%
%  See if it is a derangment.
%
    deranged = derange1_check ( n, a );

    if ( deranged )
      break;
    end

  end

  numder = numder + 1;

  if ( maxder <= numder )
    more = false;
  end

  return
end
