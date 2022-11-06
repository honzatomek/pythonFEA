function ng = polygon_grid_count ( n, nv )

%*****************************************************************************80
%
%% polygon_grid_count() counts the grid points inside a polygon.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    24 April 2015
%
%  Author:
%
%    John Burkardt
%
%  Input:
%
%    integer N, the number of subintervals on a side.
%
%    integer NV, the number of vertices.
%    3 <= NV.
%
%  Output:
%
%    integer NG, the number of grid points.
%
  ng = 1 + nv * ( n * ( n + 1 ) ) / 2;

  return
end
