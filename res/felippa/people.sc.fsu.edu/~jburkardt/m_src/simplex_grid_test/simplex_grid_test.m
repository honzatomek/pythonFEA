function simplex_grid_test ( )

%*****************************************************************************80
%
%% simplex_grid_test() tests simplex_grid().
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    16 March 2019
%
%  Author:
%
%    John Burkardt
%
  addpath ( '../simplex_grid' );

  timestamp ( );
  fprintf ( 1, '\n' );
  fprintf ( 1, 'simplex_grid_test():\n' );
  fprintf ( 1, '  MATLAB/Octave version %s\n', version ( ) );
  fprintf ( 1, '  Test simplex_grid().\n' );

  simplex_grid_test01 ( );
  simplex_grid_test02 ( );
  simplex_grid_test03 ( );
  simplex_grid_test04 ( );
  simplex_grid_test05 ( );
  simplex_grid_test06 ( );
%
%  Terminate.
%
  fprintf ( 1, '\n' );
  fprintf ( 1, 'simplex_grid_test():\n' );
  fprintf ( 1, '  Normal end of execution.\n' );
  fprintf ( 1, '\n' );
  timestamp ( );

  rmpath ( '../simplex_grid' );

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

