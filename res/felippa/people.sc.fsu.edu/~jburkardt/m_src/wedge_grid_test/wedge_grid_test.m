function wedge_grid_test ( )

%*****************************************************************************80
%
%% wedge_grid_test() tests wedge_grid().
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    07 February 2019
%
%  Author:
%
%    John Burkardt
%
  addpath ( '../wedge_grid' );

  timestamp ( );
  fprintf ( 1, '\n' );
  fprintf ( 1, 'wedge_grid_test():\n' );
  fprintf ( 1, '  MATLAB/Octave version %s.\n', version ( ) );
  fprintf ( 1, '  Test wedge_grid().\n' );

  wedge_grid_test01 ( );
  wedge_grid_test02 ( );
%
%  Terminate.
%
  fprintf ( 1, '\n' );
  fprintf ( 1, 'wedge_grid_test():\n' );
  fprintf ( 1, '  Normal end of execution.\n' );
  fprintf ( 1, '\n' );
  timestamp ( );

  rmpath ( '../wedge_grid' );

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
