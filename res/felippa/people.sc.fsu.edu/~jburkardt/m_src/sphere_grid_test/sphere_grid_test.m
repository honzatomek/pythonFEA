function sphere_grid_test ( )

%*****************************************************************************80
%
%% sphere_grid_test() tests sphere_grid().
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    25 March 2019
%
%  Author:
%
%    John Burkardt
%
  addpath ( '../sphere_grid' );

  timestamp ( )
  fprintf ( 1, '\n' );
  fprintf ( 1, 'sphere_grid_test():\n' );
  fprintf ( 1, '  MATLAB/Octave version %s\n', version ( ) );
  fprintf ( 1, '  Test sphere_grid().\n' );

  sphere_grid_test01 ( );
  sphere_grid_test02 ( );
  sphere_grid_test03 ( );
  sphere_grid_test04 ( );
  sphere_grid_test05 ( );
  sphere_grid_test06 ( );
  sphere_grid_test07 ( );
  sphere_grid_test08 ( );
  sphere_grid_test09 ( );
  sphere_grid_test10 ( );
  sphere_grid_test11 ( );
  sphere_grid_test12 ( );
  sphere_grid_test13 ( );
%
%  Terminate.
%
  fprintf ( 1, '\n' );
  fprintf ( 1, 'sphere_grid_test():\n' );
  fprintf ( 1, '  Normal end of execution.\n' );
  fprintf ( 1, '\n' );
  timestamp ( );

  rmpath ( '../sphere_grid' );

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

