function hand_plot ( )

%*****************************************************************************80
%
%% hand_plot() plots the hand data.
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
  fprintf ( 1, 'hand_plot()\n' );
  fprintf ( 1, '  Read the hand data and plot it.\n' );
%
%  Read the data.
%
  xy = load ( 'hand_nodes.txt' );
%
%  Make XY an array of column vectors.
%
  xy = xy';
%
%  Repeat the first column at the end so the polygon closes.
%
  xy = [ xy, [ xy(:,1) ] ];
%
%  Clear the graphics frame.
%
  clf ( );

  plot ( xy(1,:), xy(2,:), 'Color', 'r', 'LineWidth', 2 );
  hold ( 'on' );
  plot ( xy(1,:), xy(2,:), 'b.', 'MarkerSize', 15 );
  axis ( 'equal' );
  grid ( 'on' );
  title ( 'Hand data and straight line interpolant', 'fontsize', 16 )

  hold ( 'off' );

  pause ( 5 );

  filename = 'hand_plot.png';
  print ( '-dpng', filename );
  fprintf ( 1, '  Graphics saved as "%s"\n', filename );
%
%  Terminate.
%
  fprintf ( 1, '\n' );
  fprintf ( 1, 'hand_plot:\n' );
  fprintf ( 1, '  Normal end of execution.\n' );

  return
end
