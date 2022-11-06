function florida_point_display ( lon, lat, filename, plot_title )

%*****************************************************************************80
%
%% florida_point_display() displays points within the boundaries of Florida.
%
%  Discussion:
%
%    The points are assumed to be given in terms of longitude and latitude.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    28 June 2016
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
  fprintf ( 1, 'FLORIDA_POINT_DISPLAY:\n' );
  fprintf ( 1, '  MATLAB/Octave version %s\n', version ( ) );
  fprintf ( 1, '  Display points within the borders of Florida.\n' );
%
%  Open a new graphics figure.
%
  figure ( )
%
%  Create a map axis suitable for Florida.
%
  ax = usamap ( 'Florida' );
%
%  SHAPEREAD() reads the shapes of the US states.
%  We can use the Selector option to guarantee that only Florida is read.
%
  florida_lo = shaperead ( 'usastatelo', ...
    'UseGeoCoords', true,...
    'Selector', { @ ( name ) strcmpi( name, 'Florida' ), 'Name' } );
%
%  The Florida structure includes a longitude/latitude bounding box.
%
  lon_min = florida_lo(1).BoundingBox(1,1);
  lon_max = florida_lo(1).BoundingBox(2,1);
  lat_min = florida_lo(1).BoundingBox(1,2);
  lat_max = florida_lo(1).BoundingBox(2,2);
%
%  Display the outline, and fill with a green with some blue.
%
  geoshow ( florida_lo, 'FaceColor', [ 0.3, 1.0, 0.675 ] );
%
%  Mark each center.
%  Why, why, must I give LATITUDE FIRST HERE?
%  BECAUSE THAT"S THE INSANE CONVENTION FOR PLOTM!
%
  plotm ( lat, lon, 'r.', 'Markersize', 15 );

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
