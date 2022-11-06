function density = p0110_density ( m, n, point )

%*****************************************************************************80
%
%% p10_density returns the density for problem 10.
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
%  Reference:
%
%    Per-Olof Persson and Gilbert Strang,
%    A Simple Mesh Generator in MATLAB,
%    SIAM Review,
%    Volume 46, Number 2, June 2004, pages 329-345.
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
%    real DENSITY(N), the mesh density at each point.
%
  density = ones ( 1, n );

  return
end
