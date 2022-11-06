function voronoi_test03 ( )

%*****************************************************************************80
%
%% voronoi_test03() tests delaunay() + voronoi().
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    23 April 2005
%
%  Author:
%
%    John Burkardt
%
  fprintf ( 1, '\n' );
  fprintf ( 1, 'VORONOI_TEST03:\n' );
  fprintf ( 1, '  This script tests voronoi() + delaunay().\n' );

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
%  1. Points.
%
  fprintf ( 1, '\n' );
  fprintf ( 1, 'Plot #1: The points:\n' );

  scatter ( p(:,1), p(:,2), 'b', 'filled' );
  axis square
  axis ( [ -0.25, 1.25, -0.25, 1.25 ] );

  pause ( 5 )
%
%  2. Points + Voronoi.
%
  fprintf ( 1, '\n' );
  fprintf ( 1, 'Plot #2: The Voronoi diagram by itself:\n' );

  scatter ( p(:,1), p(:,2), 'b', 'filled' );
  axis square
  axis ( [ -0.25, 1.25, -0.25, 1.25 ] );
  hold ( 'on' );
  
  pause ( 5 )

  [ vx, vy ] = voronoi ( p(:,1), p(:,2) );
  plot ( vx, vy, '-', 'LineWidth', 3, 'Color', 'k' );

  pause ( 5 )
  hold off
%
%  3. Points + Delaunay.
%
  fprintf ( 1, '\n' );
  fprintf ( 1, 'Plot #3: The Delaunay triangulation by itself:\n' );
  
  scatter ( p(:,1), p(:,2), 'b', 'filled' );
  axis square
  axis ( [ -0.25, 1.25, -0.25, 1.25 ] );
  hold ( 'on' );
  
  pause ( 5 )

  tri = delaunay ( p(:,1), p(:,2) );
  trimesh ( tri, p(:,1), p(:,2), 'LineWidth', 3, 'Color', 'r' )
  scatter ( p(:,1), p(:,2), 'b', 'filled' );

  pause ( 5 )
  hold off
%
%  4. Points + Voronoi + Delaunay.
%
  fprintf ( 1, '\n' );
  fprintf ( 1, 'Plot #4: Points + Voronoi + Delaunay:\n' );
  
  scatter ( p(:,1), p(:,2), 'b', 'filled' );
  axis square
  axis ( [ -0.25, 1.25, -0.25, 1.25 ] );
  hold ( 'on' );
  
  pause ( 5 )
  
  [ vx, vy ] = voronoi ( p(:,1), p(:,2) );
  plot ( vx, vy, '-', 'LineWidth', 3, 'Color', 'k' );
  
  pause ( 5 )
  
  tri = delaunay ( p(:,1), p(:,2) );
  trimesh ( tri, p(:,1), p(:,2), 'LineWidth', 3, 'Color', 'r' )
  scatter ( p(:,1), p(:,2), 'b', 'filled' );

  pause ( 5 )
  hold off
%
%  5. Points + Delaunay + Voronoi.
%
  fprintf ( 1, '\n' );
  fprintf ( 1, 'Plot #5: Points + Delaunay + Voronoi:\n' );
  
  scatter ( p(:,1), p(:,2), 'b', 'filled' );
  axis square
  axis ( [ -0.25, 1.25, -0.25, 1.25 ] );
  hold ( 'on' );
  
  pause ( 5 )
  
  tri = delaunay ( p(:,1), p(:,2) );
  trimesh ( tri, p(:,1), p(:,2), 'LineWidth', 3, 'Color', 'r' )
  scatter ( p(:,1), p(:,2), 'b', 'filled' );

  pause ( 5 )
  
  [ vx, vy ] = voronoi ( p(:,1), p(:,2) );
  plot ( vx, vy, '-', 'LineWidth', 3, 'Color', 'k' );

  pause ( 5 )
  hold off
  
  return
end
