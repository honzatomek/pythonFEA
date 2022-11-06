function inside = p03_inside ( m, n, point )

%*****************************************************************************80
%
%% p03_inside reports if a point is inside the region in problem 03.
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
  r2 = 0.4;
  x1 = -1.0;
  x2 = +1.0;
  y1 = -1.0;
  y2 = +1.0;

  inside(1:n) =                                  ...
      x1 <= point(1,1:n) && point(1,1:n) <= x2 &&  ...
      y1 <= point(2,1:n) && point(2,1:n) <= y2 &&  ...
      r2 * r2 <= ( point(1,1:n) - center(1) )**2 ...
               + ( point(2,1:n) - center(2) )**2;

  return
end
