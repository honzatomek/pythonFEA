function hand_star ( )

%*****************************************************************************80
%
%% hand_star() plots the hand data.
%
%  Discussion:
%
%     This program assumes that the file 'hand_nodes.txt' is available.
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
%  Reference:
%
%    Cleve Moler,
%    Numerical Computing with MATLAB,
%    SIAM, 2004,
%    ISBN13: 978-0-898716-60-3,
%    LC: QA297.M625. 
%
  fprintf ( 1, '\n' );
  fprintf ( 1, 'hand_star\n' );

  star = [ 0.45, 0.23 ];
%
%  Read the data.
%
  xy = load ( 'hand_nodes.txt' );
%
%  Make XY an array of column vectors.
%
  xy = xy';
  [ m, n ] = size ( xy );
  xy = xy(1:2,1:2:n);
  [ m, n ] = size ( xy );
%
%  Repeat the first column at the end so the polygon closes.
%
  xy = [ xy, [ xy(:,1) ] ];
%
%  Clear the graphics frame.
%
  clf

  plot ( xy(1,:), xy(2,:), 'Color', 'r', 'LineWidth', 2 );
  hold on;
  plot ( xy(1,:), xy(2,:), 'b.', 'MarkerSize', 15 );
  axis equal
  grid on
  for i = 1 : n
    line ( [ star(1), xy(1,i) ], [ star(2), xy(2,i) ] );
    dx = xy(1,i) - star(1);
    dy = xy(2,i) - star(2);
    a = atan2 ( dy, dx ) * 180 / pi;
    fprintf ( 1, '  %2d  %10f\n', i, a );
  end
  title ( 'star: hand data polygon is star shaped!', 'fontsize', 16 )

  hold off

  pause ( 5 );

  filename = 'hand_star.png';
  print ( '-dpng', filename );
  fprintf ( 1, '  Graphics saved as "%s"\n', filename );
%
%  Terminate.
%
  fprintf ( 1, '\n' );
  fprintf ( 1, 'hand_star:\n' );
  fprintf ( 1, '  Normal end of execution.\n' );

  return
end
