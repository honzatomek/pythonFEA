function ng = triangle_grid_count ( n )

%*****************************************************************************80
%
%% triangle_grid_count() counts the grid points inside a triangle.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    05 September 2010
%
%  Author:
%
%    John Burkardt
%
%  Input:
%
%    integer N, the number of subintervals.
%
%  Output:
%
%    integer NG, the number of grid points inside the triangle.
%
  ng = ( ( n + 1 ) * ( n + 2 ) ) / 2;

  return
end
