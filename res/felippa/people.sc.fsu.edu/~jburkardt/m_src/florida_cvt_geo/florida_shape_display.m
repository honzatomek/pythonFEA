function florida_shape_display ( lon, lat, filename, plot_title )

%*****************************************************************************80
%
%% florida_shape_display() displays the shape of Florida.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    04 November 2016
%
%  Author:
%
%    John Burkardt
%
%  Input:
%
%    real LON(*), LAT(*), the longitude and latitude of the points.
%
%    string FILENAME, a name for the graphics file.
%    If FILENAME is '', the plot is not saved.
%
%    string PLOT_TITLE, a title for the plot.
%
  fprintf ( 1, '\n' );
  fprintf ( 1, 'FLORIDA_SHAPE_DISPLAY():\n' );
  fprintf ( 1, '  MATLAB/Octave version %s\n', version ( ) );
  fprintf ( 1, '  Display the shape of Florida.\n' );
%
%  Open a new graphics figure.
%
  figure ( );

  plot ( lat, lon, 'r-', 'Markersize', 15 );
  axis ( 'equal' );
  grid ( 'on' );
  xlabel ( 'Longitude' );
  ylabel ( 'Latitude' );
  title ( plot_title );
%
%  If FILENAME is not empty, save a copy of the plot.
%
  if ( ~ isempty ( filename ) )
    fprintf ( 1, '\n' );
    fprintf ( 1, '  Plot saved as "%s"\n', filename );
    print ( '-dpng', filename )
  end

  return
end
