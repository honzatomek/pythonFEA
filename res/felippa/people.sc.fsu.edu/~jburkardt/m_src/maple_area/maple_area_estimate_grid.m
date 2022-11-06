function estimate = maple_area_estimate_grid ( b, p_m, p_n, n )

%*****************************************************************************80
%
%% maple_area_estimate_grid() estimate area using a grid.
%
%  Discussion:
%
%    As a set of MATLAB coordinates, the leaf lies inside the box
%    [0,P_M] x [0,P_N].
%
%    Generate a grid of points (X,Y), spaced 1 unit apart in X and Y, 
%    from 0.5 to 399.5.
%
%    For each point (X,Y), determine if it is inside or outside the polygon.
%
%    The percentage of inside points gives the relative area of the leaf.
%
%    Brian Hayes used a grid of 1024 x 1024 points, and got a relative area
%    of 0.4185.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    07 August 2016
%
%  Author:
%
%    John Burkardt
%
%  Input:
%
%    integer B(*,2), a list of pixels that form the boundary.
%
%    integer P_M, P_N, the number of rows and columns of pixels.
%
%    integer N, the number of grid points to use in each direction.
%
%  Output:
%
%    real ESTIMATE, the relative area estimate.
%
  dx =  p_m / ( n + 1 );
  gx = linspace ( 0.0 + 0.5 * dx, p_m - 0.5 * dx, n );
  dy = p_n / ( n + 1 );
  gy = linspace ( 0.0 + 0.5 * dy, p_n - 0.5 * dy, n );

  [ XG, YG ] = meshgrid ( gx, gy );
  inside = inpolygon ( XG, YG, b(:,1), b(:,2) ); 
  estimate = sum ( sum ( inside ) ) / n / n;

  return
end

