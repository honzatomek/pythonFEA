function sphere_exactness_test ( )

%*****************************************************************************80
%
%% sphere_exactness_test() tests sphere_exactness().
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    25 March 2019
%
%  Author:
%
%    John Burkardt
%
  addpath ( '../sphere_exactness' )

  timestamp ( );
  fprintf ( 1, '\n' );
  fprintf ( 1, 'sphere_exactness_test():\n' );
  fprintf ( 1, '  MATLAB/Octave version %s\n', version ( ) );
  fprintf ( 1, '  Test sphere_exactness().\n' );

  sphere_exactness ( 'xyz1', 'design_04', 5 );

  sphere_exactness ( 'dtpw', 'lebedev_013', 5 );
%
%  Terminate.
%
  fprintf ( 1, '\n' );
  fprintf ( 1, 'sphere_exactness_test():\n' );
  fprintf ( 1, '  Normal end of execution.\n' );
  fprintf ( 1, '\n' );
  timestamp ( );

  rmpath ( '../sphere_exactness' )

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

