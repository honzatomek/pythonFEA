function [ a, more, h, t, n2, more2 ] = subcompnz_next ( n, k, a, more, ...
  h, t, n2, more2 )

%*****************************************************************************80
%
%% subcompnz_next() computes the next subcomposition of N into K nonzero parts.
%
%  Discussion:
%
%    A composition of the integer N into K nonzero parts is an ordered sequence
%    of K positive integers which sum to a value of N.
%
%    A subcomposition of the integer N into K nonzero parts is a composition
%    of M into K nonzero parts, where 0 < M <= N.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license. 
%
%  Modified:
%
%    06 June 2015
%
%  Author:
%
%    John Burkardt
%
%  Input:
%
%    integer N, the integer whose subcompositions are desired.
%
%    integer K, the number of parts in the subcomposition.
%    K must be no greater than N.
%
%    integer A(K), the parts of the subcomposition.
%
%    logical MORE, set to FALSE by the user to start the computation.
%
%    integer H, T, N2, internal parameters needed for the
%    computation.  The user may need to initialize these before the
%    very first call, but these initial values are not important.
%    The user should not alter these parameters once the computation
%    begins.
%
%    logical MORE2, an internal parameter needed for the
%    computation.  The user may need to initialize this before the
%    very first call, but the initial value is not important.
%    The user should not alter this parameter once the computation
%    begins.
%
%  Output:
%
%    integer A(K), the parts of the subcomposition.
%
%    logical MORE, set to FALSE by the routine to terminate the 
%    computation.
%
%    integer H, T, N2, updated values.
%
%    logical MORE2, an updated value.
%
  if ( n < k )
    more = false;
    a(1:k) = -1;
    return
  end
%
%  The first computation.
%
  if ( ~ more )

    a(1:k) = 1;
    more = true;
    h = 0;
    t = 0;
    n2 = k;
    more2 = false;
%
%  Do the next element at the current value of N.
%
  elseif ( more2 )

    [ a, more2, h, t ] = compnz_next ( n2, k, a, more2, h, t );

  else

    more2 = false;
    n2 = n2 + 1;

    [ a, more2, h, t ] = compnz_next ( n2, k, a, more2, h, t );

  end
%
%  Termination occurs if MORE2 = FALSE and N2 = N.
%
  if ( ~ more2 && n2 == n )
    more = false;
  end

  return
end
