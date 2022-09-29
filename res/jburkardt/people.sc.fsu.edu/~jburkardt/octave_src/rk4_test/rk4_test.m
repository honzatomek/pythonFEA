function rk4_test ( )

%*****************************************************************************80
%
%% rk4_test tests rk4.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    22 March 2020
%
%  Author:
%
%    John Burkardt
%
  addpath ( '../rk4' );

  timestamp ( );
  fprintf ( 1, '\n' );
  fprintf ( 1, 'rk4_test\n' );
  fprintf ( 1, '  MATLAB/Octave version %s\n', version ( ) );
  fprintf ( 1, '  Test rk4.\n' );

  rk4_predator_test ( );
%
%  Terminate.
%
  fprintf ( 1, '\n' );
  fprintf ( 1, 'rk4_test\n' );
  fprintf ( 1, '  Normal end of execution.\n' );
  fprintf ( 1, '\n' );
  timestamp ( );

  rmpath ( '../rk4' );

  return
end
