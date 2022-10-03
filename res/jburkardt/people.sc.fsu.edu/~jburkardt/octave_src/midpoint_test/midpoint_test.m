function midpoint_test ( )

%*****************************************************************************80
%
%% midpoint_test tests midpoint.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    07 April 2021
%
%  Author:
%
%    John Burkardt
%
  addpath ( '../midpoint' );

  timestamp ( );
  fprintf ( 1, '\n' );
  fprintf ( 1, 'midpoint_test:\n' );
  fprintf ( 1, '  MATLAB/Octave version %s\n', version ( ) );
  fprintf ( 1, '  Test midpoint() on several ODE''s.\n' );
  fprintf ( 1, '  We use a constant stepsize.\n' );

  tspan = [ 0.0, 5.0 ];
  y0 = [ 5000, 100 ];
  n = 200;
  predator_prey_midpoint_test ( tspan, y0, n );

  tspan = [ 0.0, 1.0 ];
  y0 = 0.0;
  n = 27;
  stiff_midpoint_test ( tspan, y0, n );
%
%  Terminate.
%
  fprintf ( 1, '\n' );
  fprintf ( 1, 'midpoint_test:\n' );
  fprintf ( 1, '  Normal end of execution.\n' );
  fprintf ( 1, '\n' );
  timestamp ( );

  rmpath ( '../midpoint' )

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

