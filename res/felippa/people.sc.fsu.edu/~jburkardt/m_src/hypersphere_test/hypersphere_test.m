function hypersphere_test ( )

%*****************************************************************************80
%
%% hypersphere_test() tests hypersphere().
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    01 February 2019
%
%  Author:
%
%    John Burkardt
%
  addpath ( '../hypersphere' );

  timestamp ( );
  fprintf ( 1, '\n' );
  fprintf ( 1, 'hypersphere_test():\n' );
  fprintf ( 1, '  MATLAB/Octave version %s\n', version ( ) );
  fprintf ( 1, '  Test hypersphere().\n' );

  hypersphere_test01 ( );
  hypersphere_test02 ( );
  hypersphere_test03 ( );
  hypersphere_test04 ( );
  hypersphere_test05 ( );
  hypersphere_test06 ( );
%
%  Terminate.
%
  fprintf ( 1, '\n' );
  fprintf ( 1, 'hypersphere_test():\n' );
  fprintf ( 1, '  Normal end of execution.\n' );
  fprintf ( 1, '\n' );
  timestamp ( );

  rmpath ( '../hypersphere' );

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

