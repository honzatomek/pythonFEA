function s = subset_random ( n )

%*****************************************************************************80
%
%% subset_random() returns a random subset.
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
%  Input:
%
%    integer N, the size of the set.
%
%  Output:
%
%    integer S(N), defines the subset using 0 and 1 values.
%
  s = randi ( [ 0, 1 ], n, 1 );

  return
end
