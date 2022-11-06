function triangulation_test ( )

%*****************************************************************************80
%
%% triangulation_test() tests triangulation().
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    19 January 2021
%
%  Author:
%
%    John Burkardt
%
  addpath ( '../triangulation' );

  timestamp ( );
  fprintf ( 1, '\n' );
  fprintf ( 1, 'triangulation_test():\n' );
  fprintf ( 1, '  MATLAB/Octave version %s\n', version ( ) );
  fprintf ( 1, '  Test triangulation()\n' );

  triangulation_test01 ( );
  triangulation_test02 ( );
  triangulation_test025 ( );
  triangulation_test026 ( );
  triangulation_test03 ( );
  triangulation_test04 ( );
  triangulation_test05 ( );
  triangulation_test06 ( );
  triangulation_test07 ( );
  triangulation_test08 ( );
  triangulation_test09 ( );

  triangulation_test10 ( );
  triangulation_test11 ( );
  triangulation_test12 ( );
  triangulation_test125 ( );
  triangulation_test127 ( );
  triangulation_test13 ( );
  triangulation_test14 ( );
  triangulation_test15 ( );
  triangulation_test16 ( );
  triangulation_test17 ( );
  triangulation_test18 ( );
  triangulation_test19 ( );

  triangulation_test20 ( );
  triangulation_test21 ( );
  triangulation_test213 ( );
  triangulation_test215 ( );
  triangulation_test217 ( );
  triangulation_test219 ( );
  triangulation_test22 ( );
  triangulation_test23 ( );
  triangulation_test24 ( );
  triangulation_test25 ( );
  triangulation_test26 ( );
  triangulation_test265 ( );
  triangulation_test27 ( );

  triangulation_test31 ( );
  triangulation_test32 ( );
  triangulation_test33 ( );
%
%  Terminate.
%
  fprintf ( 1, '\n' );
  fprintf ( 1, 'triangulation_test():\n' );
  fprintf ( 1, '  Normal end of execution.\n' );
  fprintf ( 1, '\n' );
  timestamp ( );

  rmpath ( '../triangulation' );

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

