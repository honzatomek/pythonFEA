function polygon_grid_test ( )

%*****************************************************************************80
%
%% polygon_grid_test() tests polygon_grid().
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    26 February 2019
%
%  Author:
%
%    John Burkardt
%
  addpath ( '../polygon_grid' );

  timestamp ( );
  fprintf ( 1, '\n' );
  fprintf ( 1, 'polygon_grid_test():\n' );
  fprintf ( 1, '  MATLAB/Octave version %s\n', version ( ) );
  fprintf ( 1, '  Test polygon_grid().\n' );

  polygon_grid_count_test ( );

  polygon_grid_points_test01 ( );
  polygon_grid_points_test02 ( );
  polygon_grid_points_test03 ( );
%
%  Terminate.
%
  fprintf ( 1, '\n' );
  fprintf ( 1, 'polygon_grid_test():\n' );
  fprintf ( 1, '  Normal end of execution.\n' );
  fprintf ( 1, '\n' );
  timestamp ( );

  rmpath ( '../polygon_grid' );

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

