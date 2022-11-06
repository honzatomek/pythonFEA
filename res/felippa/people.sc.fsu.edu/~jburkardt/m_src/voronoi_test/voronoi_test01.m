function voronoi_test01 ( )

%*****************************************************************************80
%
%% voronoi_test01() tests voronoi().
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    05 November 2008
%
%  Author:
%
%    John Burkardt
%
  fprintf ( 1, '\n' );
  fprintf ( 1, 'voronoi_test01():\n' );
  fprintf ( 1, '  Apply voronoi to a set of 9 points.\n' );

  p = [ 0.0  0.0;
        0.0  1.0;
        0.2  0.5;
        0.3  0.6;
        0.4  0.5;
        0.6  0.3;
        0.6  0.5;
        1.0  0.0;
        1.0  1.0 ];
%
%  Plot the points.
%
  scatter ( p(:,1), p(:,2), 'b', 'filled' );
  hold ( 'on' );
  axis square
  axis ( [ -1, 2, -1, 2 ] );
  title ( '9 points.' );
  hold off
  
  pause ( 5 )
%
%  Plot the points and their convex hull.
%
  scatter ( p(:,1), p(:,2), 'b', 'filled' );
  hold ( 'on' );
  k = convhull ( p(:,1), p(:,2) );
  kn = length ( k );
  line ( p(k,1), p(k,2) );
  axis square
  axis ( [ -1, 2, -1, 2 ] );
  title ( '9 points, Convex hull.' );
  hold off
  
  pause ( 5 )
%
%  Plot the points and the Delaunay triangulation.
%
  scatter ( p(:,1), p(:,2), 'b', 'filled' );
  hold ( 'on' );
  tri = delaunay ( p(:,1), p(:,2) );
  trimesh ( tri, p(:,1), p(:,2), 'LineWidth', 2, 'Color', 'r' );
  scatter ( p(:,1), p(:,2), 'b', 'filled' );
  axis square
  axis ( [ -1, 2, -1, 2 ] );
  title ( '9 points, Delaunay triangulation' );
  hold off
  
  pause ( 5 )
%
%  Plot the points and the Voronoi diagram created by VORONOI.
%
  [ vx, vy ] = voronoi ( p(:,1), p(:,2) );

  scatter ( p(:,1), p(:,2), 'b', 'filled' );
  hold ( 'on' );
  plot ( vx, vy, '-', 'LineWidth', 2, 'Color', 'r' );
  axis square
  axis ( [ -1, 2, -1, 2 ] );
  title ( '9 points, Voronoi diagram using "Voronoi"' );
  hold off
  
  pause ( 5 )

  return
end
