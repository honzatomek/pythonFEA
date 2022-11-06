function [ p, more, rank ] = perm1_next3 ( n, p, more, rank )

%*****************************************************************************80
%
%% perm1_next3() computes permutations of (1,...,N).
%
%  Discussion:
%
%    The routine is initialized by calling with MORE = TRUE, in which case
%    it returns the identity permutation.
%
%    If the routine is called with MORE = FALSE, then the successor of the
%    input permutation is computed.
%
%    Trotter's algorithm is used.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    09 November 2018
%
%  Author:
%
%    Original FORTRAN77 version by Hale Trotter,
%    MATLAB version by John Burkardt
%
%  Reference:
%
%    Hale Trotter,
%    PERM, Algorithm 115,
%    Communications of the Association for Computing Machinery,
%    Volume 5, 1962, pages 434-435.
%
%  Input:
%
%    Input, integer N, the number of objects being permuted.
%
%    integer P(N), not needed on first call.  On repeated
%    calls this is simply the output value from the previous call.
%
%    logical MORE, set to FALSE on an initialization call.  On repeated
%    calls this is simply the output value from the previous call.
%
%    integer  RANK, the rank of the current permutation.  Not
%    needed on the first call.  On repeated
%    calls this is simply the output value from the previous call.
%
%  Output:
%
%    integer P(N), the next permutation; if MORE is FALSE, 
%    then P is the first permutation in the sequence.
%
%    logical MORE, is TRUE if there was a next
%    permutation to produce, or FALSE if there are no more permutations 
%    to produce.
%
%    integer  RANK, the rank of the output permutation.
%
  if ( ~ more )

    p = 1 : n;
    more = true;
    rank = 1;

  else

    n2 = n;
    m2 = rank;
    s = n;

    while ( true )

      q = mod ( m2, n2 );
      t = mod ( m2, 2 * n2 );

      if ( q ~= 0 )
        break
      end

      if ( t == 0 )
        s = s - 1;
      end

      m2 = floor ( m2 / n2 );
      n2 = n2 - 1;

      if ( n2 == 0 )
        p = 1 : n;
        more = false;
        rank = 1;
        break
      end

    end

    if ( n2 ~= 0 )

      if ( q == t )
        s = s - q;
      else
        s = s + q - n2;
      end

      t      = p(s);
      p(s)   = p(s+1);
      p(s+1) = t;

      rank = rank + 1;

    end

  end

  return
end

