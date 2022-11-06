function check = partn_sf_check ( n, nmax, npart, a )

%*****************************************************************************80
%
%% partn_sf_check() checks an SF partition of an integer with largest entry NMAX.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    26 January 2011
%
%  Author:
%
%    John Burkardt
%
%  Reference:
%
%    Donald Kreher, Douglas Simpson,
%    Combinatorial Algorithms,
%    CRC Press, 1998,
%    ISBN: 0-8493-3988-X,
%    LC: QA164.K73.
%
%  Input:
%
%    integer N, the integer to be partitioned.
%    N must be positive.
%
%    integer NMAX, the value of the largest entry.
%    1 <= NMAX <= N.
%
%    integer NPART, the number of parts of the partition.
%    1 <= NPART <= N.
%
%    integer A(NPART), contains the partition.
%    A(1) through A(NPART) contain the nonzero integers which
%    sum to N.  The entries must be in DESCENDING order.
%
%  Output:
%
%    integer CHECK.
%    1, the data is legal.
%    0, the data is not legal.
%
  check = 1;

  if ( n < 1 )
    check = 0;
    return
  end

  if ( nmax < 1 | n < nmax )
    check = 0;
    return
  end

  if ( npart < 1 | n < npart )
    check = 0;
    return
  end
%
%  Entry 1 must be NMAX.
%
  if ( a(1) ~= nmax )
    check = 0;
    return
  end
%
%  Every entry must lie between 1 and N.
%
  for i = 1 : npart
    if ( a(i) < 1 | n < a(i) )
      check = 0;
      return
    end
  end
%
%  The entries must be in descending order.
%
  for i = 2 : npart
    if ( a(i-1) < a(i) )
      check = 0;
      return
    end
  end
%
%  The entries must add up to N.
%
  asum = sum ( a(1:npart) );
  if ( asum ~= n )
    check = 0;
  end

  return
end
