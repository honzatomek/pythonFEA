function predator_prey_ode_test ( )

%*****************************************************************************80
%
%% predator_prey_ode_test tests predator_prey_ode.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    29 October 2020
%
%  Author:
%
%    John Burkardt
%
  addpath ( '../predator_prey_ode' )

  timestamp ( );
  fprintf ( 1, '\n' );
  fprintf ( 1, 'predator_prey_ode_test:\n' );
  fprintf ( 1, '  MATLAB/Octave version %s\n', version ( ) );
  fprintf ( 1, '  Test predator_prey_ode using euler, midpoint, ode23, ode45.\n' );

  [ alpha, beta, gamma, delta ] = predator_prey_parameters ( );

  fprintf ( 1, '\n' );
  fprintf ( 1, '  parameters:\n' );
  fprintf ( 1, '    alpha = %g\n', alpha );
  fprintf ( 1, '    beta  = %g\n', beta );
  fprintf ( 1, '    gamma = %g\n', gamma );
  fprintf ( 1, '    delta = %g\n', delta );

  p0 = [ 5000, 100 ];
  timespan = [ 0.0, 5.0 ];
  n = 200;

  predator_prey_euler ( timespan, p0, n );
  predator_prey_midpoint ( timespan, p0, n );
  predator_prey_ode23 ( timespan, p0 );
  predator_prey_ode45 ( timespan, p0 );
%
%  Terminate.
%
  fprintf ( 1, '\n' );
  fprintf ( 1, 'predator_prey_ode_test:\n' );
  fprintf ( 1, '  Normal end of execution.\n' );
  fprintf ( 1, '\n' );
  timestamp ( );

  rmpath ( '../predator_prey_ode' )

  return
end
