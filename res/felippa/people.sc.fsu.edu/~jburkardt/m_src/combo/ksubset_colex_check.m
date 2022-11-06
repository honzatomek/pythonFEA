function check = ksubset_colex_check ( k, n, t )

%*****************************************************************************80
%
%% ksubset_colex_check() checks a K subset in colex form.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    27 December 2015
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
%    integer K, the number of elements each K subset must
%    have. 0 <= K <= N.
%
%    integer N, the number of elements in the master set.
%    0 <= N.
%
%    integer T(K), describes a K subset.  T(I) is the I-th
%    element of the K subset.  The elements must be listed in
%    DESCENDING order.
%
%  Output:
%
%    integer CHECK.
%    1, the data is legal.
%    0, the data is not legal.
%
  check = 1;

  if ( n < 0 )
    check = 0;
    return
  end

  if ( k < 0 | n < k )
    check = 0;
    return
  end
%
%  Check that entries are in descending order, and between 1 and N.
%
  tmax = n + 1;

  for i = 1 : k

    if ( t(i) <= 0 | tmax <= t(i) )
      check = 0;
      return
    end

    tmax = t(i);

  end

  return
end
