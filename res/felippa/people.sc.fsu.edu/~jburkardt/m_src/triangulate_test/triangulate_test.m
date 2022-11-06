function triangulate_test ( )

%*****************************************************************************80
%
%% triangulate_test() tests triangulate().
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    08 April 2019
%
%  Author:
%
%    John Burkardt
%
  addpath ( '../triangulate' )

  timestamp ( );
  fprintf ( 1, '\n' );
  fprintf ( 1, 'triangulate_test():\n' );
  fprintf ( 1, '  MATLAB/Octave version %s\n', version ( ) );
  fprintf ( 1, '  Test triangulate().\n' );

  triangulate (  'snake', 'no' );
%
%  Terminate.
%
  fprintf ( 1, '\n' );
  fprintf ( 1, 'triangulate_test():\n' );
  fprintf ( 1, '  Normal end of execution.\n' );
  fprintf ( 1, '\n' );
  timestamp ( );

  rmpath ( '../triangulate' )

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
