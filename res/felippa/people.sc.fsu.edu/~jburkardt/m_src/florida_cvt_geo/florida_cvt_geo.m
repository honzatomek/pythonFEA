function [ gen_lon, gen_lat ] = florida_cvt_geo ( gen_lon, gen_lat )

%*****************************************************************************80
%
%% florida_cvt_geo() estimates a CVT for Florida based on geometric data.
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
%    real GEN_LON(N), GEN_LAT(N), points to be used as the initial
%    generators.
%
%  Output:
%
%    real GEN_LON(N), GEN_LAT(N), improved estimates of the CVT
%    generators.
%
  fprintf ( 1, '\n' );
  fprintf ( 1, 'FLORIDA_CVT_GEO:\n' );
  fprintf ( 1, '  MATLAB/Octave version %s\n', version ( ) );
  fprintf ( 1, '  Estimate a centroidal Voronoi tessellation (CVT)\n' );
  fprintf ( 1, '  of Florida, using geometric data.\n' );
%
%  Destroy all row vectors.
%
  gen_lon = gen_lon ( : );
  gen_lat = gen_lat ( : );
%
%  Select N random points within the Florida polygon to use as generators.
%
  n = length ( gen_lon );
%
%  Display the points.
%
  filename = '';
  plot_title = 'Random starting points';
  florida_point_display ( gen_lon, gen_lat, filename, plot_title );
%
%  Prepare for random sampling.
%
  sample_num = 5000;

  for step = 1 : 10

    [ cen_lon, cen_lat ] = florida_centroid_geo ( gen_lon, gen_lat, sample_num );

    plot_title = sprintf ( 'CVT Step %d', step );
    florida_voronoi_display ( gen_lon, gen_lat, cen_lon, cen_lat, '', plot_title );

    filename = '';
    plot_title = sprintf ( 'Generators after step %d\n', step );
    florida_point_display ( cen_lon, cen_lat, filename, plot_title );

    gen_lon = cen_lon;
    gen_lat = cen_lat;

  end

  return
end
