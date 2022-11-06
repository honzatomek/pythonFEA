function disk_monte_carlo_test ( )

%*****************************************************************************80
%
%% disk_monte_carlo_test() tests disk_monte_carlo().
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    07 January 2019
%
%  Author:
%
%    John Burkardt
%
  addpath ( '../disk_monte_carlo' );

  timestamp ( );
  fprintf ( 1, '\n' );
  fprintf ( 1, 'disk_monte_carlo_test():\n' );
  fprintf ( 1, '  MATLAB/Octave version %s\n', version ( ) );
  fprintf ( 1, '  Test disk_monte_carlo().\n' );

  disk_area_test ( );

  center = [ 0.0, 0.0 ];
  r = 1.0;
  disk_sample_test ( center, r );

  center = [ 1.0, 0.0 ];
  r = 1.0;
  disk_sample_test ( center, r );

  center = [ 1.0, 2.0 ];
  r = 3.0;
  disk_sample_test ( center, r );
%
%  Terminate.
%
  fprintf ( 1, '\n' );
  fprintf ( 1, 'disk_monte_carlo_test():\n' );
  fprintf ( 1, '  Normal end of execution.\n' );
  fprintf ( 1, '\n' );
  timestamp ( );

  rmpath ( '../disk_monte_carlo' );

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

