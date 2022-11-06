function xy = circle_arc_grid ( r, c, a, n )

%*****************************************************************************80
%
%% circle_arc_grid() computes grid points along a circular arc.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license. 
%
%  Modified:
%
%    29 October 2022
%
%  Author:
%
%    John Burkardt
%
%  Input:
%
%    real R, the radius of the circle.
%
%    real C(2), the coordinates of the center.
%
%    real A(2), the angle of the first and last points, in DEGREES.
%
%    integer N, the number of points.
%
%  Output:
%
%    real XY(N,2), the grid points.
%
  xy = zeros ( n, 2 );

  aj = linspace ( a(1), a(2), n )';

  xy(1:n,1) = c(1) + r * cos ( aj(1:n) * pi / 180.0 );
  xy(1:n,2) = c(2) + r * sin ( aj(1:n) * pi / 180.0 );

  return
end
