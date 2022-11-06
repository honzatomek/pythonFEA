function check = ksubset_lex_check ( k, n, t )

%*****************************************************************************80
%
%% ksubset_lex_check() checks a K subset in lex form.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    24 January 2011
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
%    integer K, the number of elements each K subset must have. 
%    0 <= K <= N.
%
%    integer N, the number of elements in the master set.
%    0 <= N.
%
%    integer T(K), describes a K subset.  T(I) is the I-th
%    element of the K subset.  The elements must be listed in ASCENDING order.
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
%  Items are between 0 and N, and in ascending order?
%
  tmin = 0;

  for i = 1 : k

    if ( t(i) <= tmin | n < t(i) )
      check = 0;
      return
    end

    tmin = t(i);

  end

  return
end
