function quadrilateral_test ( )

%*****************************************************************************80
%
%% quadrilateral_test() tests quadrilateral().
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    26 April 2022
%
%  Author:
%
%    John Burkardt
%
  addpath ( '../quadrilateral' );

  timestamp ( );
  fprintf ( 1, '\n' );
  fprintf ( 1, 'quadrilateral_test():\n' );
  fprintf ( 1, '  MATLAB/Octave version %s\n', version ( ) );
  fprintf ( 1, '  quadrilateral() contains functions for geometric computations\n' );
  fprintf ( 1, '  involving quadrilaterals.\n' );

  quadrilateral_angles_test ( );
  quadrilateral_area_3d_test ( );
  quadrilateral_area_test ( );
  quadrilateral_contains_point_test ( );
  quadrilateral_is_convex_test ( );
  quadrilateral_is_simple_test ( );
  quadrilateral_perimeter_test ( );
  quadrilateral_point_dist_signed_test ( );
  quadrilateral_point_dist_test ( );
  quadrilateral_point_near_test ( );
  quadrilateral_random_test ( );
  quadrilateral_random_convex_test ( );
  quadrilateral_random_simple_test ( );
%
%  Terminate.
%
  fprintf ( 1, '\n' );
  fprintf ( 1, 'quadrilateral_test():\n' );
  fprintf ( 1, '  Normal end of execution.\n' );
  fprintf ( 1, '\n' );
  timestamp ( );

  rmpath ( '../quadrilateral' );

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

