function t = gray_code_random ( n )

%*****************************************************************************80
%
%% gray_code_random() returns a random Gray code of N digits.
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
%    integer N, the number of digits.
%
%  Output:
%
%    integer T(N), a random Gray code.
%
  t = randi ( [ 0, 1 ], 1, n );

  return
end
