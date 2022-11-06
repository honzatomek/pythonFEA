function boundary = p01_boundary_nearest ( m, n, point )

%*****************************************************************************80
%
%% p01_boundary_nearest returns a nearest boundary point in problem 01.
%
%  Discussion:
%
%    The given input point need not be inside the region.
%
%    In some cases, more than one boundary point may be "nearest",
%    but only one will be returned.
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
%    real BOUNDARY(M,N), points on the boundary
%    that are nearest to each point.
%
  center = [ 0.0, 0.0 ];
  r1 = 1.0;

  for j = 1 : n

    r = sqrt ( sum ( ( point(1:m,j) - center(1:m)' ).^2 ) );

    if ( r == 0.0 )
      boundary(1,j) = center(1) + r1;
      boundary(2,j) = center(2);
    else
      boundary(1:m,j) = center(1:m)' ...
        + ( r1 / r ) * ( point(1:m,j) - center(1:m)' );
    end

  end

  return
end
