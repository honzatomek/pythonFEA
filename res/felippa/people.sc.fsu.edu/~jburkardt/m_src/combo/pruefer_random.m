function p = pruefer_random ( n )

%*****************************************************************************80
%
%% pruefer_random() returns a random Pruefer code of length N-2.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    11 September 2022
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
%  Output:
%
%    integer P(N-2), the random Pruefer code.
%
  p = randi ( [ 1, n ], 1, n - 2 );

  return
end
