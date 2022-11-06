function check = subset_check ( n, t )

%*****************************************************************************80
%
%% subset_check() checks a subset.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    25 December 2015
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
%    integer N, the number of elements in the master set.
%    N must be positive.
%
%    integer T(N), the subset.  If T(I) = 0, item I is
%    not in the subset; if T(I) = 1, item I is in the subset.
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

  for i = 1 : n

    if ( t(i) ~= 0 & t(i) ~= 1 )
      check = 0;
      return
    end

  end

  return
end
