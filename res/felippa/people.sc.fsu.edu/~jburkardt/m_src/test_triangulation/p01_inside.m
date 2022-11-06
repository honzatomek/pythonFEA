function inside = p01_inside ( m, n, point )

%*****************************************************************************80
%
%% p01_inside reports if a point is inside the region in problem 01.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    12 April 2009
%
%  Author:
%
%    John Burkardt
%
%  Input:
%
%    integer M, the spatial dimension.
%
%    integer N, the number of points.
%
%    real POINT(M,N), the coordinates of the points.
%
%  Output:
%
%    logical INSIDE(N), is TRUE if the point is in the region.
%
  center = [ 0.0, 0.0 ];
  r1 = 1.0;

  inside(1:n) = ( ( point(1,1:n) - center(1) ).^2 ...
                + ( point(2,1:n) - center(2) ).^2 ) <= r1 * r1;

  return
end
