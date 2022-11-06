function [ lam, a, more ] = ytb_next ( n, lam, a, more )

%*****************************************************************************80
%
%% ytb_next() computes the next Young tableau for a given shape.
%
%  Discussion:
%
%    When the routine is called with MORE = FALSE (the first time), and
%    if LAM on this call has M parts, with M < N, then the user
%    must also make sure that LAM(M+1) = 0.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    22 June 2004
%
%  Author:
%
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
%    integer N, the integer which is to be partitioned.
%
%    integer LAM(N), contains a partition of N.
%    The elements of LAM are nonnegative integers that sum to N.
%    On the first call, with MORE = FALSE, the user sets LAM.
%    After the first call, the input value of LAM is not important.
%
%    integer A(N).  On the first call, with MORE = FALSE,
%    no value of A needs to be set.  After the first call, the input
%    value of A should be the output value of A from the previous call.
%
%    logical MORE.  Set MORE to FALSE before the first call.
%    Thereafter, set it to the output value of MORE on the previous call.
%
%  Output:
%
%    integer LAM(N), contains the partition of N,
%    corresponding to the Young tableau.
%
%    integer A(N), the next Young tableau.  A(I) is the
%    row containing I in the output tableau.
%
%    logical MORE, is TRUE until the last tableau is returned,
%    when the value of MORE is FALSE.
%
  it = n;

  if ( more )

    lam(1) = 1;
    lam(2:n) = 0;

    isave = 0;

    for i = 2 : n

      lam(a(i)) = lam(a(i)) + 1;

      if ( a(i) < a(i-1) )
        isave = i;
        break;
      end

    end

    if ( isave == 0 )
      more = false;
      return
    end

    it = lam(1+a(isave));

    for i = n : -1 : 1

      if ( lam(i) == it )
        a(isave) = i;
        lam(i) = lam(i) - 1;
        it = isave - 1;
        break;
      end

    end

  end

  k = 1;
  ir = 1;

  while ( true )

    if ( n < ir )
      break;
    end

    if ( lam(ir) ~= 0 )
      a(k) = ir;
      lam(ir) = lam(ir) - 1;
      k = k + 1;
      ir = ir + 1;
      continue;
    end

    if ( it < k )
      break;
    end

    ir = 1;

  end

  if ( n == 1 )
    more = false;
    return
  end

  for j = 2 : n
    if ( a(j) < a(j-1) )
      more = true;
      return
    end
  end

  more = false;

  return
end
