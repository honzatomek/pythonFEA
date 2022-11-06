function sphere_llq_grid_test ( )

%*****************************************************************************80
%
%% sphere_llq_grid_test() tests sphere_llq_grid().
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    26 March 2019
%
%  Author:
%
%    John Burkardt
%
  addpath ( '../sphere_llq_grid' );

  timestamp ( )
  fprintf ( 1, '\n' );
  fprintf ( 1, 'sphere_llq_grid_test():\n' );
  fprintf ( 1, '  MATLAB/Octave version %s\n', version ( ) );
  fprintf ( 1, '  Test sphere_llq_grid().\n' );

  sphere_llq_grid_point_count_test ( );
  sphere_llq_grid_points_test ( );
  sphere_llq_grid_line_count_test ( );
  sphere_llq_grid_lines_test ( );
  sphere_llq_grid_display_test ( );
%
%  Terminate.
%
  fprintf ( 1, '\n' );
  fprintf ( 1, 'sphere_llq_grid_test():\n' );
  fprintf ( 1, '  Normal end of execution.\n' );
  fprintf ( 1, '\n' );
  timestamp ( );

  rmpath ( '../sphere_llq_grid' );

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

