function circles_test ( )

%*****************************************************************************80
%
%% circles_test() tests CIRCLES.
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
  addpath ( '../circles' );

  timestamp ( );
  fprintf ( 1, '\n' );
  fprintf ( 1, 'circles_test():\n' );
  fprintf ( 1, '  MATLAB/Octave version %s.\n', version ( ) );
  fprintf ( 1, '  Test CIRCLES.\n' );

  circles_test01 ( );
  pause ( 5 );
  circles_test02 ( );
  pause ( 5 );
  circles_test03 ( );
  pause ( 5 );
  circles_test04 ( );
  pause ( 5 );
  circles_test05 ( );
  pause ( 5 );
  circles_test06 ( );
  pause ( 5 );
  circles_test07 ( );
  pause ( 5 );
  circles_test08 ( );
  pause ( 5 );
  circles_test09 ( );
  pause ( 5 );
  circles_test10 ( );
  pause ( 5 );
  circles_test11 ( );
  pause ( 5 );
%
%  Terminate.
%
  fprintf ( 1, '\n' );
  fprintf ( 1, 'circles_test():\n' );
  fprintf ( 1, '  Normal end of execution.\n' );
  fprintf ( 1, '\n' );
  timestamp ( );

  rmpath ( '../circles' );

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

