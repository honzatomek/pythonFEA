function rkf45_test ( )

%*****************************************************************************80
%
%% rkf45_test tests rkf45.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    07 March 2019
%
%  Author:
%
%    John Burkardt
%
  addpath ( '../rkf45' );

  timestamp ( );
  fprintf ( 1, '\n' );
  fprintf ( 1, 'rkf45_test\n' );
  fprintf ( 1, '  MATLAB/Octave version %s\n', version ( ) );
  fprintf ( 1, '  Test rkf45.\n' );

  rkf45_test04 ( );
  rkf45_test05 ( );
  rkf45_test06 ( );
%
%  Terminate.
%
  fprintf ( 1, '\n' );
  fprintf ( 1, 'rkf45_test\n' );
  fprintf ( 1, '  Normal end of execution.\n' );
  fprintf ( 1, '\n' );
  timestamp ( );

  rmpath ( '../rkf45' );

  return
end
