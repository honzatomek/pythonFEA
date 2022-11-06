function hand_linear ( )

%*****************************************************************************80
%
%% hand_linear() applies a linear transformation xy2 = A * xy + b to hand data.
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
  fprintf ( 1, 'hand_linear\n' );
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
%  Define the transformation as xy2 = A * x + b.
%
  A = [ -2.2, +1; 0.5, 0.4 ];

  b = repmat ( [ 1.5; -0.5 ], 1, 60 );
%
%  Apply the transformation.
%
  xy2 = A * xy + b;
%
%  Clear the graphics frame.
%
  clf ( )
%
%  Plot the original data.
%
  plot ( xy(1,:), xy(2,:), 'Color', 'r', 'LineWidth', 2 );
%
%  Plot the transformed data.
%
  hold on
  plot ( xy2(1,:), xy2(2,:), 'Color', 'b', 'LineWidth', 2 );
%
%  Annotate.
%
  axis equal
  grid on
  title ( 'Data (red), and A*x+b (blue)', 'fontsize', 16 )
  hold off

  pause ( 5 );

  filename = 'hand_linear.png';
  print ( '-dpng', filename );
  fprintf ( 1, '  Graphics saved as "%s"\n', filename );
%
%  Terminate.
%
  fprintf ( 1, '\n' );
  fprintf ( 1, 'hand_linear:\n' );
  fprintf ( 1, '  Normal end of execution.\n' );

  return
end
