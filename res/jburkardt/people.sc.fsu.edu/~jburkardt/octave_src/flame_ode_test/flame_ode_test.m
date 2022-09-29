function flame_ode_test ( )

%*****************************************************************************80
%
%% flame_ode_test tests flame_ode.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    11 November 2020
%
%  Author:
%
%    John Burkardt
%
  addpath ( '../flame_ode' )

  timestamp ( );
  fprintf ( 1, '\n' );
  fprintf ( 1, 'flame_ode_test:\n' );
  fprintf ( 1, '  MATLAB/Octave version %s\n', version ( ) );
  fprintf ( 1, '  flame_ode uses an ODE to model combustion.\n' );

  n = 1000;

  flame_euler ( n );

  flame_ode45 ( );
%
%  Terminate.
%
  fprintf ( 1, '\n' );
  fprintf ( 1, 'flame_ode_test:\n' );
  fprintf ( 1, '  Normal end of execution.\n' );
  fprintf ( 1, '\n' );
  timestamp ( );

  rmpath ( '../flame_ode' )

  return
end
