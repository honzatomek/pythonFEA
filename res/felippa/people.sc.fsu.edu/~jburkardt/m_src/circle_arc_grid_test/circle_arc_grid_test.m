function circle_arc_grid_test ( )

%*****************************************************************************80
%
%% circle_arc_grid_test() tests circle_arc_grid().
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license. 
%
%  Modified:
%
%    13 December 2018
%
%  Author:
%
%    John Burkardt
%
  addpath ( '../circle_arc_grid' );

  timestamp ( );
  fprintf ( 1, '\n' );
  fprintf ( 1, 'circle_arc_grid_test():\n' );
  fprintf ( 1, '  MATLAB/Octave version %s.\n', version ( ) );
  fprintf ( 1, '  Test circle_arc_grid().\n' );

  circle_arc_grid_test01 ( );
%
%  Terminate.
%
  fprintf ( 1, '\n' );
  fprintf ( 1, 'circle_arc_grid_test()\n' );
  fprintf ( 1, '  Normal end of execution.\n' );
  fprintf ( 1, '\n' );
  timestamp ( );

  rmpath ( '../circle_arc_grid' );

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

