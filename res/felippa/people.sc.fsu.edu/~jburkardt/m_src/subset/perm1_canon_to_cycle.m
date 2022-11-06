function p2 = perm1_canon_to_cycle ( n, p1 )

%*****************************************************************************80
%
%% perm1_canon_to_cycle(): permutation of (1,...,N) from canonical to cycle form.
%
%  Example:
%
%    Input:
%
%      4 5 2 1 6 3
%
%    Output:
%
%      -4 5 -2 -1 6 3,
%      indicating the cycle structure
%      ( 4, 5 ) ( 2 ) ( 1, 6, 3 )
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license. 
%
%  Modified:
%
%    14 June 2004
%
%  Author:
%
%    John Burkardt
%
%  Reference:
%
%    Donald Knuth,
%    The Art of Computer Programming,
%    Volume 1, Fundamental Algorithms,
%    Addison Wesley, 1968, page 176.
%
%  Input:
%
%    integer N, the number of objects permuted.
%
%    integer P1(N), the permutation, in canonical form.
%
%  Output:
%
%    integer P2(N), the permutation, in cycle form.
%
  p2(1:n) = p1(1:n);

  pmin = p2(1) + 1;

  for i = 1 : n

    if ( p2(i) < pmin )
      pmin = p2(i);
      p2(i) = -p2(i);
    end

  end

  return
end
