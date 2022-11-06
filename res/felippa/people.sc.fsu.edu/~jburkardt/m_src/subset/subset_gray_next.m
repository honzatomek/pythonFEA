function [ a, more, ncard, iadd ] = subset_gray_next ( n, a, more, ncard )

%*****************************************************************************80
%
%% subset_gray_next() generates all subsets of a set of order N, one at a time.
%
%  Discussion:
%
%    This routine generates the subsets one at a time, by adding or subtracting
%    exactly one element on each step.
%
%    This uses a Gray code ordering of the subsets.
%
%    The user should set MORE = FALSE and the value of N before
%    the first call.  On return, the user may examine A which contains
%    the definition of the new subset, and must check MORE, because
%    as soon as it is FALSE on return, all the subsets have been
%    generated and the user probably should cease calling.
%
%    The first set returned is the empty set.
%
%  Example:
%
%    N = 4
%
%    0 0 0 0
%    1 0 0 0
%    1 1 0 0
%    0 1 0 0
%    0 1 1 0
%    1 1 1 0
%    1 0 1 0
%    0 0 1 0
%    0 0 1 1
%    1 0 1 1
%    1 1 1 1
%    0 1 1 1
%    0 1 0 1
%    1 1 0 1
%    1 0 0 1
%    0 0 0 1
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    28 June 2004
%
%  Author:
%
%    Original FORTRAN77 version by Albert Nijenhuis, Herbert Wilf.
%    MATLAB version by John Burkardt.
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
%    integer A(N), the value of A on the previous call.
%    This value is not needed on the first call, with MORE = FALSE.
%
%    logical MORE, should be set to FALSE on the first call, and
%    then set to TRUE for all subsequent calls.
%
%    integer NCARD, the cardinality of A.  This value is not needed
%    on the first call, with MORE = FALSE.
%
%  Output:
%
%    integer A(N), the Gray code for the next subset.  A(I) = 0
%    if element I is in the subset, 1 otherwise.
%
%    logical MORE. will be returned TRUE until all the subsets
%    have been generated.
%
%    integer NCARD, the cardinality of A.
%
%    integer IADD, the element which was added or removed to the
%    previous subset to generate the current one.  Exception:
%    the empty set is returned on the first call, and IADD is set to -1.
%

%
%  The first set returned is the empty set.
%
  if ( ~ more )

    a(1:n) = 0;
    more = true;
    ncard = 0;
    iadd = -1;

  else

    a(1:n) = a(1:n);
    iadd = 1;

    if ( mod ( ncard, 2 ) ~= 0 )

      while ( true )

        iadd = iadd + 1;
        if ( a(iadd-1) ~= 0 )
          break
        end

      end

    end

    a(iadd) = 1 - a(iadd);
    ncard = ncard + 2 * a(iadd) - 1;
%
%  The last set returned is the singleton A(N).
%
    if ( ncard == a(n) )
      more = false;
    end

  end

  return
end
 
