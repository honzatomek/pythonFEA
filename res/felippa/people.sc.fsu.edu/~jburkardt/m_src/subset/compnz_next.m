function [ a, more, h, t ] = compnz_next ( n, k, a, more, h, t )

%*****************************************************************************80
%
%% compnz_next() computes the compositions of the integer N into K nonzero parts.
%
%  Discussion:
%
%    A composition of the integer N into K nonzero parts is an ordered sequence
%    of K positive integers which sum to N.  The compositions (1,2,1)
%    and (1,1,2) are considered to be distinct.
%
%    The routine computes one composition on each call until there are no more.
%    For instance, one composition of 6 into 3 parts is 3+2+1, another would
%    be 4+1+1 but 5+1+0 is not allowed since it includes a zero part.
%
%    On the first call to this routine, set MORE = FALSE.  The routine
%    will compute the first element in the sequence of compositions, and
%    return it, as well as setting MORE = TRUE.  If more compositions
%    are desired, call again, and again.  Each time, the routine will
%    return with a new composition.
%
%    However, when the LAST composition in the sequence is computed
%    and returned, the routine will reset MORE to FALSE, signaling that
%    the end of the sequence has been reached.
%
%  Example:
%
%    The 10 compositions of 6 into three nonzero parts are:
%
%      4 1 1,  3 2 1,  3 1 2,  2 3 1,  2 2 2,  2 1 3,
%      1 4 1,  1 3 2,  1 2 3,  1 1 4.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    01 December 2005
%
%  Author:
%
%    John Burkardt
%
%  Reference:
%
%    Albert Nijenhuis and Herbert Wilf,
%    Combinatorial Algorithms,
%    Academic Press, 1978, second edition,
%    ISBN 0-12-519260-6.
%
%  Input:
%
%    integer N, the integer whose compositions are desired.
%
%    integer K, the number of parts in the composition.
%    K must be no greater than N.
%
%    integer A(K).  On input, the previous composition.  
%    On the first call, with MORE = FALSE, set A = [].
%
%    logical MORE.  The input value of MORE on the first
%    call should be FALSE, which tells the program to initialize.
%    On subsequent calls, MORE should be TRUE, or simply the
%    output value of MORE from the previous call.
%
%    integer H, T, two integers that must be declared by
%    the user and initialized to 0.  
%
%  Output:
%
%    integer A(K), the next composition.
%
%    logical MORE, TRUE unless the composition that is being returned is the 
%    final one in the sequence.
%
%    integer H, T, values which should not be changed by the user.
%

%
%  We use the trick of computing ordinary compositions of (N-K)
%  into K parts, and adding 1 to each part.
%
  if ( n < k )
    more = false;
    a(1:k) = -1;
    return
  end

  if ( ~ more )

    t = n - k;
    h = 0;
    a(1) = n - k;
    a(2:k) = 0;

  else

    a(1:k) = a(1:k) - 1;

    if ( 1 < t )
      h = 0;
    end

    h = h + 1;
    t = a(h);
    a(h) = 0;
    a(1) = t - 1;
    a(h+1) = a(h+1) + 1;

  end

  more = ( a(k) ~= ( n - k ) );

  a(1:k) = a(1:k) + 1;

  return
end
