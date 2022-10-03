function backward_euler_test ( )

%*****************************************************************************80
%
%% backward_euler_test() tests backward_euler().
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    11 October 2020
%
%  Author:
%
%    John Burkardt
%
  addpath ( '../backward_euler' );

  timestamp ( );
  fprintf ( 1, '\n' );
  fprintf ( 1, 'backward_euler_test():\n' );
  fprintf ( 1, '  MATLAB/Octave version %s\n', version ( ) );
  fprintf ( 1, '  Test backward_euler().\n' );

  tspan = [ 0.0, 5.0 ];
  y0 = [ 5000, 100 ];
  n = 200;
  predator_prey_backward_euler ( tspan, y0, n );

  tspan = [ 0.0, 1.0 ];
  y0 = 0.0;
  n = 27;
  stiff_backward_euler ( tspan, y0, n );
%
%  Terminate.
%
  fprintf ( 1, '\n' );
  fprintf ( 1, 'backward_euler_test():\n' );
  fprintf ( 1, '  Normal end of execution.\n' );
  fprintf ( 1, '\n' );
  timestamp ( );

  rmpath ( '../backward_euler' )

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

