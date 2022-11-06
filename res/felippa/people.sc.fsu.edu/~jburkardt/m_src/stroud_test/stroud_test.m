function stroud_test ( )

%*****************************************************************************80
%
%% stroud_test() tests stroud().
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license. 
%
%  Modified:
%
%    22 March 2019
%
%  Author:
%
%    John Burkardt
%
  addpath ( '../stroud' );

  timestamp ( );
  fprintf ( 1, '\n' );
  fprintf ( 1, 'stroud_test():\n' );
  fprintf ( 1, '  MATLAB/Octave version %s\n', version ( ) );
  fprintf ( 1, '  Test stroud().\n' );

  stroud_test01 ( );
  stroud_test02 ( );
  stroud_test03 ( );
  stroud_test04 ( );
  stroud_test045 ( );
  stroud_test05 ( );
  stroud_test052 ( );
  stroud_test054 ( );
  stroud_test07 ( );
  stroud_test08 ( );
  stroud_test085 ( );
  stroud_test09 ( );

  stroud_test10 ( );
  stroud_test11 ( );
  stroud_test12 ( );
  stroud_test13 ( );
  stroud_test14 ( );
  stroud_test15 ( );
  stroud_test16 ( );
  stroud_test163 ( );
  stroud_test165 ( );
  stroud_test167 ( );
  stroud_test17 ( );
  stroud_test18 ( );
  stroud_test19 ( );
  
  stroud_test20 ( );
  stroud_test205 ( );
  stroud_test207 ( );
  stroud_test2075 ( );
  stroud_test208 ( );
  stroud_test21 ( );
  stroud_test215 ( );
  stroud_test22 ( );
  stroud_test23 ( );
  stroud_test24 ( );
  stroud_test25 ( );
  stroud_test255 ( );
  stroud_test26 ( );
  stroud_test27 ( );
  stroud_test28 ( );
  stroud_test29 ( );

  stroud_test30 ( );
  stroud_test31 ( );
  stroud_test32 ( );
  stroud_test322 ( );
  stroud_test324 ( );
  stroud_test326 ( );
  stroud_test33 ( );
  stroud_test335 ( );
  stroud_test34 ( );
  stroud_test345 ( );
  stroud_test35 ( );
  stroud_test36 ( );
  stroud_test37 ( );
  stroud_test38 ( );
  stroud_test39 ( );

  stroud_test40 ( );
  stroud_test41 ( );
  stroud_test42 ( );
  stroud_test425 ( );
  stroud_test43 ( );
  stroud_test44 ( );
  stroud_test45 ( );
  stroud_test46 ( );
  stroud_test47 ( );
  stroud_test48 ( );
  stroud_test49 ( );
%
%  Terminate.
%
  fprintf ( 1, '\n' );
  fprintf ( 1, 'stroud_test():\n' );
  fprintf ( 1, '  Normal end of execution.\n' );
  fprintf ( 1, '\n' );
  timestamp ( );

  rmpath ( '../stroud' );

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

