function circle_monte_carlo_test ( )

%*****************************************************************************80
%
%% circle_monte_carlo_test() tests circle_monte_carlo().
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
  addpath ( '../circle_monte_carlo' );

  timestamp ( );
  fprintf ( 1, '\n' );
  fprintf ( 1, 'circle_monte_carlo_test():\n' );
  fprintf ( 1, '  MATLAB/Octave version %s.\n', version ( ) );
  fprintf ( 1, '  Test CIRCLE_MONTE_CARLO.\n' );

  circle01_sample_random_test ( );
  circle01_sample_ergodic_test ( );
%
%  Terminate.
%
  fprintf ( 1, '\n' );
  fprintf ( 1, 'CIRCLE_MONTE_CARLO_TEST\n' );
  fprintf ( 1, '  Normal end of execution.\n' );
  fprintf ( 1, '\n' );
  timestamp ( );

  rmpath ( '../circle_monte_carlo' );

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

