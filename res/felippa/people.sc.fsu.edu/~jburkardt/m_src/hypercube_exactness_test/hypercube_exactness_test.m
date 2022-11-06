function hypercube_exactness_test ( )

%*****************************************************************************80
%
%% hypercube_exactness_test() tests hypercube_exactness().
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
  addpath ( '../hypercube_exactness' )

  timestamp ( );
  fprintf ( 1, '\n' );
  fprintf ( 1, 'hypercube_exactness_test():\n' );
  fprintf ( 1, '  MATLAB/Octave version %s\n', version ( ) );
  fprintf ( 1, '  Test hypercube_exactness().\n' );

  hypercube_exactness ( 'cc_d1_o2', 5 );
%
%  Terminate.
%
  fprintf ( 1, '\n' );
  fprintf ( 1, 'hypercube_exactness_test():\n' );
  fprintf ( 1, '  Normal end of execution.\n' );
  fprintf ( 1, '\n' );
  timestamp ( );

  rmpath ( '../hypercube_exactness' )

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

