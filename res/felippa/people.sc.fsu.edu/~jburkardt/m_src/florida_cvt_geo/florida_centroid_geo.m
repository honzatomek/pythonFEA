function [ cen_lon, cen_lat ] = florida_centroid_geo ( gen_lon, ...
  gen_lat, sample_num )

%*****************************************************************************80
%
%% florida_centroid_geo() estimates geometric centroids of Voronoi regions.
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
%    real GEN_LON(*), GEN_LAT(*), the longitude and latitude 
%    of generators to be displayed on a map of Florida.
%
%    integer SAMPLE_NUM, the number of samples to use.
%
%  Output:
%
%    real CEN_LON(*), CEN_LAT(*), the longitude and latitude 
%    of the centroids of the Voronoi regions.
%
  fprintf ( 1, '\n' );
  fprintf ( 1, 'FLORIDA_CENTROID_GEO:\n' );
  fprintf ( 1, '  MATLAB version\n' );
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

  lon_min = florida_lo(1).BoundingBox(1,1);
  lon_max = florida_lo(1).BoundingBox(2,1);
  lat_min = florida_lo(1).BoundingBox(1,2);
  lat_max = florida_lo(1).BoundingBox(2,2);
%
%  Initialize the counts for each generator.
%
  n = length ( gen_lon );
  cen_lat = zeros ( n, 1 );
  cen_lon = zeros ( n, 1 );
  cen_count = zeros ( n, 1 );
%
%  Choose random LL points in the bounding box.
%  Accept them only if they are contained in the Florida polygon.
%
  for s = 1 : sample_num
    inside = 0;
    while ( ~ inside )
      lat = lat_min + ( lat_max - lat_min ) * rand ( 1, 1 );
      lon = lon_min + ( lon_max - lon_min ) * rand ( 1, 1 );
      inside = inpolygon ( lat, lon, florida_lo(1).Lat(1:570), florida_lo(1).Lon(1:570) );

      if ( inside )
        d_min = Inf;
        i_min = 0;
        for i = 1 : n
          d = ( lat - gen_lat(i) ) ^ 2 + ( lon - gen_lon(i) ) ^ 2;
          if ( d < d_min )
            d_min = d;
            i_min = i;
          end
        end
        cen_lat(i_min) = cen_lat(i_min) + lat;
        cen_lon(i_min) = cen_lon(i_min) + lon;
        cen_count(i_min) = cen_count(i_min) + 1;
        break
      end

    end
  end
%
%  Set the centroid estimates.
%
  for i = 1 : n
    if ( cen_count(i) == 0 )
      cen_lon(i) = gen_lon(i);
      cen_lat(i) = gen_lat(i);
    else
      cen_lon(i) = cen_lon(i) / cen_count(i);
      cen_lat(i) = cen_lat(i) / cen_count(i);
    end
  end

  return
end

