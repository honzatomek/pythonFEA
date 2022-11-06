function check = pruefer_check ( n, p )

%*****************************************************************************80
%
%% pruefer_check() checks a Pruefer code.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    25 January 2011
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
%    integer N, the number of nodes in the tree.
%    N must be at least 3.
%
%    integer P(N-2), the Pruefer code for the tree.
%
%  Output:
%
%    integer CHECK.
%    1, the data is legal.
%    0, the data is not legal.
%
  check = 1;

  if ( n < 3 )
    check = 0;
    return
  end

  for i = 1 : n - 2
    if ( p(i) < 1 | n < p(i) )
      check = 0;
      return
    end
  end

  return
end
