function polygon_test ( )

%*****************************************************************************80
%
%% polygon_test() tests polygon().
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    27 April 2022
%
%  Author:
%
%    John Burkardt
%
  addpath ( '../polygon' );

  timestamp ( );
  fprintf ( 1, '\n' );
  fprintf ( 1, 'polygon_test():\n' );
  fprintf ( 1, '  MATLAB/Octave version %s\n', version ( ) );
  fprintf ( 1, '  Test polygon()\n' );

  polygon_angles_test ( );
  polygon_area_test ( );
  polygon_area_3d_test ( );
  polygon_area_lattice_test ( );
  polygon_centroid_test ( );
  polygon_centroid_3d_test ( );
  polygon_contains_point_test ( );
  polygon_data_test ( );
  polygon_diameter_test ( );
  polygon_expand_test ( );
  polygon_integral_test ( );
  polygon_is_convex_test ( );
  polygon_normal_3d_test ( );
  polygon_perimeter_test ( );
  polygon_perimeter_quad_test ( );
  polygon_point_dist_test ( );
  polygon_point_near_test ( );
  polygon_sample_test ( );
  polygon_solid_angle_3d_test ( );
  polygon_triangulate_test ( );
%
%  Terminate.
%
  fprintf ( 1, '\n' );
  fprintf ( 1, 'polygon_test():\n' );
  fprintf ( 1, '  Normal end of execution.\n' );
  fprintf ( 1, '\n' );
  timestamp ( );

  rmpath ( '../polygon' );

  return
end
function timestamp ( )

%*****************************************************************************80
%
%% timestamp() prints the current YMDHMS date as a timestamp.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    14 February 2003
%
%  Author:
%
%    John Burkardt
%
  t = now;
  c = datevec ( t );
  s = datestr ( c, 0 );
  fprintf ( 1, '%s\n', s );

  return
end

