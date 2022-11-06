function g = simplex_grid_index_sample ( m, n )

%*****************************************************************************80
%
%% simplex_grid_index_sample() returns a random simplex grid index.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    30 July 2014
%
%  Author:
%
%    John Burkardt
%
%  Input:
%
%    integer M, the spatial dimension.
%
%    integer N, the number of subintervals in each dimension.
%
%  Output:
%
%    integer G(M+1), a randomly selected index in the simplex grid.
%
  g = comp_random ( n, m + 1 );
  g = g(:);

  return
end
