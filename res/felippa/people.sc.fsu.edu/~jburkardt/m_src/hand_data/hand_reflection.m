function hand_reflection ( )

%*****************************************************************************80
%
%% hand_reflection() reflects the data and plots the result.
%
%  Discussion:
%
%     This program assumes that the file 'HAND_NODES.TXT' is available.
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
  fprintf ( 1, 'hand_reflection\n' );
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
%  Define the transformations as xy2 = A2 * xy, xy3 = A3 * xy
%  where A2 reflects the x value, and A3 reflects the y value.
%
  A2 = [ -1.0, 0.0; 0.0,  1.0 ];
  A3 = [  1.0, 0.0; 0.0, -1.0 ];
%
%  Apply the transformations.
%  
  xy2 = A2 * xy;
  xy3 = A3 * xy;
%
%  Clear the graphics frame.
%
  clf
%
%  Plot the original data.
%
  plot ( xy(1,:), xy(2,:), 'Color', 'r', 'LineWidth', 2 );
%
%  Plot the transformed data.
%
  hold on
  plot ( xy2(1,:), xy2(2,:), 'Color', 'b', 'LineWidth', 2 );
  plot ( xy3(1,:), xy3(2,:), 'Color', 'g', 'LineWidth', 2 );
%
%  Annotate.
%
  axis equal
  grid on
  title ( 'Data (red), x-reflected (blue), y-reflected (green)', 'fontsize', 16 )

  hold off

  pause ( 5 );

  filename = 'hand_reflection.png';
  print ( '-dpng', filename );
  fprintf ( 1, '  Graphics saved as "%s"\n', filename );
%
%  Terminate.
%
  fprintf ( 1, '\n' );
  fprintf ( 1, 'hand_reflection:\n' );
  fprintf ( 1, '  Normal end of execution.\n' );

  return
end
