function [ lat, lon ] = florida_shape_read ( )

%*****************************************************************************80
%
%% florida_shape_read() extracts the low resolution Florida polygon.
%
%  Discussion:
%
%    The MATLAB Mapping Toolbox includes information about high and
%    low resolution polygonal models of US States.  We want to examine
%    the low resolution 570 vertex polygonal model of Florida.
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
%  Output:
%
%    real LON(570), LAT(570), the longitude and latitude of the
%    polygonal vertices, listed in counterclockwise order.
%
  fprintf ( 1, '\n' );
  fprintf ( 1, 'FLORIDA_SHAPE_READ:\n' );
  fprintf ( 1, '  MATLAB/Octave version %s\n', version ( ) );
  fprintf ( 1, '  Estimate centroids of Voronoi regions, given generators.\n' );
%
%  SHAPEREAD() reads the shapes of the US states.
%  We will use the "low resolution" data.
%  We will ask for "geo coords", that is, longitude and latitude.
%  Because of our bounding box, only Florida will be set up.
%
%  By the way, for no discernible or sensible reason, the following command
%  will not be be understood if I write "strcmpi (" instead of "strcmpi(".
%  A language in which spacing between tokens matters?  Just that one particular
%  space?  Please explain!  No, please don't!.
%
  florida_lo = shaperead ( 'usastatelo', ...
    'UseGeoCoords', true,...
    'Selector', { @ ( name ) strcmpi( name, 'Florida' ), 'Name' } );

  lat = florida_lo(1).Lat(:);
  lon = florida_lo(1).Lon(:);

  return
end

