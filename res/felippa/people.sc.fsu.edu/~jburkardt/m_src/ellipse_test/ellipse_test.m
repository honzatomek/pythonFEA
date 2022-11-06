function ellipse_test ( )

%*****************************************************************************80
%
%% ellipse_test() tests ellipse().
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    14 October 2022
%
%  Author:
%
%    John Burkardt
%
  addpath ( '../ellipse' );

  timestamp ( );
  fprintf ( 1, '\n' );
  fprintf ( 1, 'ellipse_test():\n' );
  fprintf ( 1, '  MATLAB/Octave version %s\n', version ( ) );
  fprintf ( 1, '  Test ellipse()\n' );

  ellipse_area1_test ( );
  ellipse_area2_test ( );
  ellipse_area3_test ( );
  ellipse_aspect_ratio_test ( );
  ellipse_eccentricity_test ( );
  ellipse_flattening_test ( );
  ellipse_point_dist_2d_test ( );
  ellipse_point_near_2d_test ( );
  ellipse_points_2d_test ( );
  ellipse_points_arc_2d_test ( );
  ellipse_quadratic_to_standard_test ( );
  ellipse_standard_to_quadratic_test ( );

  ellipsoid_area_test ( );
%
%  Terminate.
%
  fprintf ( 1, '\n' );
  fprintf ( 1, 'ellipse_test():\n' );
  fprintf ( 1, '  Normal end of execution.\n' );
  fprintf ( 1, '\n' );
  timestamp ( );

  rmpath ( '../ellipse' );

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

