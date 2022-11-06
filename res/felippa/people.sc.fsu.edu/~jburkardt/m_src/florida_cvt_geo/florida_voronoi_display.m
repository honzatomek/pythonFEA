function florida_voronoi_display ( gen_lon, gen_lat, lon, lat, filename, ...
  plot_title )

%*****************************************************************************80
%
%% florida_voronoi_display() displays generators, and their Voronoi diagram.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    03 July 2016
%
%  Author:
%
%    John Burkardt
%
%  Input:
%
%    real GEN_LON(*), GEN_LAT(*), the longitude and latitude of 
%    generators to be displayed on a map of Florida, using black dots.
%
%    real LON(*), LAT(*), additional points, to be shown using
%    red dots.
%
%    string FILENAME, a name for the graphics file.
%    If FILENAME is '', the plot is not saved.
%
%    string PLOT_TITLE, a title for the plot.
%
  if ( nargin < 4 )
    cen_lon = [];
    cen_lat = [];
    filename = '';
    plot_title = '';
  end

  fprintf ( 1, '\n' );
  fprintf ( 1, 'FLORIDA_VORONOI_DISPLAY:\n' );
  fprintf ( 1, '  MATLAB version\n' );
  fprintf ( 1, '  Display Voronoi diagram of Florida,\n' );
  fprintf ( 1, '  with generator points and Voronoi diagram.\n' );
%
%  Destroy all row vectors!
%
  gen_lon = gen_lon ( : );
  gen_lat = gen_lat ( : );
%
%  Open a new graphics figure.
%
  figure ( );
%
%  Create a map axis suitable for Florida.
%
  ax = usamap ( 'Florida' );
%
%  SHAPEREAD() reads the shapes of the US states.
%  Using the Selector function, we can guarantee only Florida will be read.
%
  florida_lo = shaperead ( 'usastatelo', ...
    'UseGeoCoords', true,...
    'Selector', { @ ( name ) strcmpi( name, 'Florida' ), 'Name' } );
%
%  The structure for Florida includes a bounding box.
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
%  Mark each generator.
%
  gen_num = length ( gen_lat );

  plotm ( gen_lat, gen_lon, 'k.', 'Markersize', 15 );

% for i = 1 : gen_num
%   textm ( gen_lat(i), gen_lon(i), tag(i), 'fontweight', 'bold' )
% end
%
%  Mark each auxilliary point.
%
  if ( ~ isempty ( lat ) )
    plotm ( lat, lon, 'r.', 'Markersize', 15 )
  end
%
%  To patch MATLAB's lazy Voronoi routine, we're just going to surround
%  our data by a frame of auxilliary points, far enough away not to
%  interfere too much with the internal lines.
%
  lat_max = lat_max + 2.0;
  lat_min = lat_min - 2.0;
  lon_max = lon_max + 2.0;
  lon_min = lon_min - 2.0;

  lon2 = gen_lon;
  lat2 = gen_lat;
  for i = 1 : 10
    x = lon_min;
    y = lat_max + ( i / 10.0 ) * ( lat_min - lat_max );
    lon2 = [ lon2; x ];
    lat2 = [ lat2; y ];
  end

  for i = 1 : 10
    x = lon_min + ( i / 10.0 ) * ( lon_max - lon_min );
    y = lat_min;
    lon2 = [ lon2; x ];
    lat2 = [ lat2; y ];
  end

  for i = 1 : 10
    x = lon_max;
    y = lat_min + ( i / 10.0 ) * ( lat_max - lat_min );
    lon2 = [ lon2; x ];
    lat2 = [ lat2; y ];
  end

  for i = 1 : 10
    x = lon_max + ( i / 10.0 ) * ( lon_min - lon_max );
    y = lat_max;
    lon2 = [ lon2; x ];
    lat2 = [ lat2; y ];
  end
%
%  Now compute the (finite part of the) Voronoi diagram.
%
  [ vx, vy ] = voronoi ( lat2, lon2 );
%
%  Draw the Voronoi lines.
%
  linem ( vx, vy, 'b-' );

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

