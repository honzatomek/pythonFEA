function [ p, more ] = perm0_lex_next ( n, p, more )

%*****************************************************************************80
%
%% perm0_lex_next() generates permutations of (0,...,N-1) in lexical order.
%
%  Example:
%
%    N = 3
%
%    1   0 1 2
%    2   0 2 1
%    3   1 0 2
%    4   1 2 0
%    5   2 0 1
%    6   2 1 0
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    30 May 2015
%
%  Author:
%
%    John Burkardt
%
%  Reference:
%
%    Mok-Kong Shen,
%    Algorithm 202: Generation of Permutations in Lexicographical Order,
%    Communications of the ACM,
%    Volume 6, September 1963, page 517.
%
%  Input:
%
%    integer N, the number of elements being permuted.
%
%    integer P(N), the permutation, in standard index form.
%
%    logical MORE.
%    On the first call, the user should set MORE = FALSE, which signals
%    the routine to do initialization.
%    On return, if MORE is TRUE, then another permutation has been
%    computed and returned, while if MORE is FALSE, there are no more
%    permutations.
%
%  Output:
%
%    integer P(N), the next permutation.
%
%    logical MORE.
%    On the first call, the user should set MORE = FALSE, which signals
%    the routine to do initialization.
%    On return, if MORE is TRUE, then another permutation has been
%    computed and returned, while if MORE is FALSE, there are no more
%    permutations.
%

%
%  Initialization.
%
  if ( ~ more )

    p = i4vec_indicator0 ( n );
    more = true;

  else

    if ( n <= 1 )
      p = [];
      more = false;
      return
    end

    w = n;

    while ( p(w) < p(w-1) )

      if ( w == 2 )
        more = false;
        return
      end

      w = w - 1;

    end

    u = p(w-1);

    for j = n : -1 : w

      if ( u < p(j) )

        p(w-1) = p(j);
        p(j) = u;

        for k = 0 : floor ( ( n - w - 1 ) / 2 )
          t      = p(n-k);
          p(n-k) = p(w+k);
          p(w+k) = t;
        end

        return

      end

    end

  end

  return
end
