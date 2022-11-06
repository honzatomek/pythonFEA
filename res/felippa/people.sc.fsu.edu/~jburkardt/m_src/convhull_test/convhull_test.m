function convhull_test ( )

%*****************************************************************************80
%
%% convhull_test() tests convhull().
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    16 December 2018
%
%  Author:
%
%    John Burkardt
%
  timestamp ( );
  fprintf ( 1, '\n' );
  fprintf ( 1, 'convhull_test():\n' );
  fprintf ( 1, '  MATLAB/Octave version %s.\n', version ( ) );
  fprintf ( 1, '  Test convhull().\n' );

  points = eddy ( );
  convhull_test_case ( points, 'eddy' );

  points = graham ( );
  convhull_test_case ( points, 'graham' );

  points = kn57 ( );
  convhull_test_case ( points, 'kn57' );

  points = rand ( 30, 2 );
  convhull_test_case ( points, 'random' );
%
%  Terminate.
%
  fprintf ( 1, '\n' );
  fprintf ( 1, 'convhull_test():\n' );
  fprintf ( 1, '  Normal end of execution.\n' );
  fprintf ( 1, '\n' );
  timestamp ( );

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

