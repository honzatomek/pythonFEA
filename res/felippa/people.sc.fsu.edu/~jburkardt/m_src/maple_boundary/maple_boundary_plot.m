function maple_boundary_plot ( b )

%*****************************************************************************80
%
%% maple_boundary_plot() plots the polygonal boundary of the maple leaf.
%
%  Discussion:
%
%    The sequence of integer coordinates in the array B(*,2) constitutes
%    the outline of a maple leaf as determined by MATLAB.
%
%    This function produces a plot of just that outline.
%
%    It also has to reflect the X coordinate, which somehow got reversed
%    in the previous processing, and it has to convince MATLAB to draw
%    the plot with the same physical coordinates for X and Y.
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
  fprintf ( 1, '\n' );
  fprintf ( 1, 'maple_boundary_plot():\n' );
  fprintf ( 1, '  Display a plot of the boundary of the maple leaf.\n' );
%
%  Plot the polygonal boundary of the leaf.
%
  figure ( );
  plot ( b(:,1), b(:,2), 'LineWidth', 2 );
  title ( 'Maple boundary line plot' );
%
%  Neither the AXES command nor the AXIS EQUAL command does what any
%  sensible person could ask, namely, to make the graph appear in a box
%  of exactly the shape requested.  For some obscure reason, AXIS SQUARE
%  does the job...today.
%
  axis ( 'square' );
%
%  Save the image.
%
  filename = 'maple_boundary.png';
  print ( '-dpng', filename );
  fprintf ( 1, '\n' );
  fprintf ( 1, '  Boundary plot saved as "%s"\n', filename );

  return
end

