function check = cycle_check ( n, ncycle, t, index )

%*****************************************************************************80
%
%% cycle_check() checks a permutation in cycle form.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    26 November 2015
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
%    integer N, the number of items permuted.
%    N must be positive.
%
%    integer NCYCLE, the number of cycles.
%    1 <= NCYCLE <= N.
%
%    integer T(N), INDEX(NCYCLE), describes the permutation
%    as a collection of NCYCLE cycles.
%
%  Output:
%
%    integer CHECK, error flag.
%    1, the data is legal.
%    0, the data is not legal.
%
  check = 1;
%
%  N must be at least 1.
%
  if ( n < 1 )
    check = 0;
    return
  end
%
%  1 <= NCYCLE <= N.
%
  if ( ncycle < 1 | n < ncycle )
    check = 0;
    return
  end
%
%  1 <= INDEX(I) <= N.
%
  for i = 1 : ncycle
    if ( index(i) < 1 | n < index(i) )
      check = 0;
      return
    end
  end
%
%  The INDEX values sum to N.
%
  if ( sum ( index(1:ncycle) ) ~= n )
    check = 0;
    return
  end
%
%  1 <= T(I) <= N.
%
  for i = 1 : n
    if ( t(i) < 1 | n < t(i) )
      check = 0;
      return
    end
  end
%
%  Verify that every value from 1 to N occurs in T.
%
  for iseek = 1 : n

    ifind = 0;

    for i = 1 : n
      if ( t(i) == iseek )
        ifind = i;
        break;
      end
    end

    if ( ifind == 0 )
      check = 0;
      return
    end

  end

  return
end
