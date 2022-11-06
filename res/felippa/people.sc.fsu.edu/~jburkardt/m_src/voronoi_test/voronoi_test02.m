function voronoi_test02 ( )

%*****************************************************************************80
%
%% voronoi_test02() displays a Voronoi diagram for 50 points.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    04 November 2008
%
%  Author:
%
%    John Burkardt
%
  fprintf ( 1, '\n' );
  fprintf ( 1, 'voronoi_test02():\n' );
  fprintf ( 1, '  Demonstrate what a Voronoi diagram\n' );
  fprintf ( 1, '  looks like for a "large" set of points,\n' );
  fprintf ( 1, '  here chosen uniformly at random in the unit square.\n' );

  x = rand ( 50, 1 );
  y = rand ( 50, 1 );
%
%  Here, we are using an improved version of MATLAB's voronoi command.
%  You can replace this by plain old "voronoi", if you like.
%
  [ vx, vy ] = voronoi ( x, y );

  scatter ( x, y, 'b', 'filled' );
  axis square
  axis ( [ -0.1, 1.1, -0.1, 1.1 ] );
  title ( '50 random points' );
  hold off
  
  pause ( 5 )
%
%  Plot the points and their convex hull.
%
  scatter ( x, y, 'b', 'filled' );
  hold ( 'on' );
  k = convhull ( x(:), y(:) );
  kn = length ( k );
  line ( x(k), y(k) );
  axis square
  axis ( [ -0.1, 1.1, -0.1, 1.1 ] );
  title ( 'Convex hull of 50 random points.' );
  hold off
  
  pause ( 5 )
%
%  Plot the points and the Delaunay triangulation.
%
  scatter ( x, y, 'b', 'filled' );
  hold ( 'on' );
  tri = delaunay ( x, y );
  trimesh ( tri, x, y, 'LineWidth', 2, 'Color', 'r' );
  scatter ( x, y, 'b', 'filled' );
  axis square
  axis ( [ -0.1, 1.1, -0.1, 1.1 ] );
  title ( 'Delaunay triangulation of 50 random points' );
  hold off
  
  pause ( 5 )
%
%  Plot the points and the Voronoi diagram.
%
  scatter ( x, y, 'b', 'filled' );
  hold ( 'on' );
  plot ( vx, vy, '-', 'LineWidth', 3, 'Color', 'r' );
  axis square
  axis ( [ -0.1, 1.1, -0.1, 1.1 ] );
  title ( 'Voronoi diagram of 50 random points.' );
  hold off
  
  pause ( 5 )

  return
end
