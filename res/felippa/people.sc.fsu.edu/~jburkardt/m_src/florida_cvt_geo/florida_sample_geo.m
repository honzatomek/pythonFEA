function [ s_lon, s_lat ] = florida_sample_geo ( sample_num )

%*****************************************************************************80
%
%% florida_sample_geo() produces geometrically random sample points in Florida.
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
%    integer SAMPLE_NUM, the number of sample points desired.
%
%  Output:
%
%    real S_LON(SAMPLE_NUM), S_LAT(SAMPLE_NUM), sample points.
%
  fprintf ( 1, '\n' );
  fprintf ( 1, 'FLORIDA_SAMPLE_GEO:\n' );
  fprintf ( 1, '  MATLAB/Octave version %s\n', version ( ) );
  fprintf ( 1, '  Seek sample points within Florida.\n' );
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
%
%  Extract the LL bounding box for the Florida polygon.
%
  lon_min = florida_lo(1).BoundingBox(1,1);
  lon_max = florida_lo(1).BoundingBox(2,1);
  lat_min = florida_lo(1).BoundingBox(1,2);
  lat_max = florida_lo(1).BoundingBox(2,2);
%
%  Choose random LL points in the bounding box.
%  Accept them only if they are containined in the Florida polygon.
%
  s_lon = zeros ( sample_num, 1 );
  s_lat = zeros ( sample_num, 1 );

  for i = 1 : sample_num
    inside = 0;
    while ( ~ inside )
      lat = lat_min + ( lat_max - lat_min ) * rand ( 1, 1 );
      lon = lon_min + ( lon_max - lon_min ) * rand ( 1, 1 );
      inside = inpolygon ( lon, lat, florida_lo(1).Lon, florida_lo(1).Lat );
      if ( inside )
        s_lon(i) = lon;
        s_lat(i) = lat;
        break
      end
    end
  end

  return
end
 
