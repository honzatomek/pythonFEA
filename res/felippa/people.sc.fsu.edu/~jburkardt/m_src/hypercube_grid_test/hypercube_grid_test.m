function hypercube_grid_test ( )

%*****************************************************************************80
%
%% hypercube_grid_test() tests hypercube_grid().
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    01 February 2019
%
%  Author:
%
%    John Burkardt
%
  addpath ( '../hypercube_grid' );

  timestamp ( );
  fprintf ( 1, '\n' );
  fprintf ( 1, 'hypercube_grid_test():\n' );
  fprintf ( 1, '  MATLAB/Octave version %s\n', version ( ) );
  fprintf ( 1, '  Test hypercube_grid().\n' );

  hypercube_grid_test01 ( );
  hypercube_grid_test02 ( );
  hypercube_grid_test03 ( );
%
%  Terminate.
%
  fprintf ( 1, '\n' );
  fprintf ( 1, 'hypercube_grid_test():\n' );
  fprintf ( 1, '  Normal end of execution.\n' );
  fprintf ( 1, '\n' );
  timestamp ( );

  rmpath ( '../hypercube_grid' );

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

