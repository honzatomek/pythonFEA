function rank = index_rank1 ( n, hi, a )

%*****************************************************************************80
%
%% index_rank1() ranks an index vector within given upper limits.
%
%  Example:
%
%    N = 3,
%    HI(1) = 4, HI(2) = 2, HI(3) = 3
%    A = ( 4, 1, 2 )
%
%    RANK = 12
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
%  Input:
%
%    integer N, the number of entries in A.
%
%    integer HI(N), the upper limits for the array indices.
%    The lower limit is implicitly 1, and each HI(I) should be at least 1.
%
%    integer A(N), the index to be ranked.
%
%  Output:
%
%    integer RANK, the rank of the index vector, or -1 if A
%    is not a legal index.
%
  rank = -1;
  for i = 1 : n
    if ( a(i) < 1 || hi(i) < a(i) )
      return
    end
  end

  rank = 0;
  for i = n : -1 : 1
    rank = hi(i) * rank + a(i);
  end

  rank = 1;
  range = 1;
  for i = 1 : n
    rank = rank + ( a(i) - 1 ) * range;
    range = range * hi(i);
  end

  return
end
