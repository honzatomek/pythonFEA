function euler_test ( )

%*****************************************************************************80
%
%% euler_test tests euler.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    25 April 2020
%
%  Author:
%
%    John Burkardt
%
  addpath ( '../euler' );

  timestamp ( );
  fprintf ( 1, '\n' );
  fprintf ( 1, 'euler_test\n' );
  fprintf ( 1, '  MATLAB/Octave version %s\n', version ( ) );
  fprintf ( 1, '  Test euler().\n' );

  n = 50;
  humps_euler ( n );
%
%  Terminate.
%
  fprintf ( 1, '\n' );
  fprintf ( 1, 'euler_test:\n' );
  fprintf ( 1, '  Normal end of execution.\n' );
  fprintf ( 1, '\n' );
  timestamp ( );

  rmpath ( '../euler' );

  return
end
function timestamp ( )

%*****************************************************************************80
%
%% timestamp prints the current YMDHMS date as a timestamp.
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


