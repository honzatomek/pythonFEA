function [ a, done ] = comb_next ( n, k, a, done )

%*****************************************************************************80
%
%% comb_next() computes combinations of K things out of N.
%
%  Discussion:
%
%    The combinations are computed one at a time, in lexicographical order.
%
%    10 April 1009: Thanks to "edA-qa mort-ora-y" for supplying a
%    correction to this code.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    10 April 2009
%
%  Author:
%
%    John Burkardt
%
%  Reference:
%
%    Charles Mifsud,
%    Combination in Lexicographic Order,
%    ACM algorithm 154,
%    Communications of the ACM,
%    March 1963.
%
%  Input:
%
%    integer N, the total number of things.
%
%    integer K, the number of things in each combination.
%
%    integer A(K), the output value of A on the previous call.
%    This value is not needed on a startup call.
%
%    logical DONE, should be set to TRUE on the first call,
%    and thereafter set to the output value of DONE on the previous call.
%
%  Output:
%
%    integer(K), the next combination.
%
%    logical DONE, is FALSE if the routine can be called
%    again for more combinations, and TRUE if there are no more.
%
  if ( done )

    if ( k <= 0 )
      return
    end

    a(1:k) = i4vec_indicator1 ( k );

    done = false;

  else

    if ( a(k) < n )
      a(k) = a(k) + 1;
      return
    end

    for i = k : -1 : 2

      if ( a(i-1) < n-k+i-1 )

        a(i-1) = a(i-1) + 1;

        for j = i : k
          a(j) = a(i-1) + j - ( i-1 );
        end

        return

      end

    end

    done = true;

  end

  return
end
 
