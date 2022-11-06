function hand_hull ( )

%*****************************************************************************80
%
%% hand_hull() plots the hull of the hand data.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    29 April 2019
%
%  Author:
%
%    John Burkardt
%
  fprintf ( 1, '\n' );
  fprintf ( 1, 'hand_hull:\n' );
  fprintf ( 1, '  MATLAB version.\n' );
%
%  Read the data.
%
  xy = load ( 'hand_nodes.txt' );
%
%  Plot the data and the convex hull.
%
  clf
  k = convhull ( xy(:,1), xy(:,2) );
  plot ( xy(:,1), xy(:,2), 'b.', 'MarkerSize', 20 );
  hold on
  plot( xy(k,1), xy(k,2), 'r-', 'LineWidth', 2 );
  grid on
  axis tight
  axis equal
  xlabel ( '<--- X --->', 'fontsize', 16 );
  ylabel ( '<--- Y --->', 'fontsize', 16 );
  title ( 'Convex hull of hand data', 'fontsize', 16 );
  hold off

  pause ( 5 );

  filename = 'hand_hull.png';
  print ( '-dpng', filename );
  fprintf ( 1, '  Graphics saved as "%s"\n', filename );
%
%  Terminate.
%
  fprintf ( 1, '\n' );
  fprintf ( 1, 'hand_hull:\n' );
  fprintf ( 1, '  Normal end of execution.\n' );

  return
end
