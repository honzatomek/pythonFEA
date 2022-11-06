function annulus_monte_carlo_test ( )

%*****************************************************************************80
%
%% annulus_monte_carlo_test() tests annulus_monte_carlo().
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    20 March 2021
%
%  Author:
%
%    John Burkardt
%
  addpath ( '../annulus_monte_carlo' )

  timestamp ( );
  fprintf ( 1, '\n' );
  fprintf ( 1, 'annulus_monte_carlo_test():\n' );
  fprintf ( 1, '  MATLAB/Octave version %s\n', version ( ) );
  fprintf ( 1, '  Test annulus_monte_carlo().\n' );

  annulus_area_test ( )

  center = [ 0.0, 0.0 ];
  r1 = 0.0;
  r2 = 1.0;
  annulus_sample_test ( center, r1, r2 );

  center = [ 0.0, 0.0 ];
  r1 = 0.5;
  r2 = 1.0;
  annulus_sample_test ( center, r1, r2 );

  center = [ 1.0, 0.0 ];
  r1 = 0.0;
  r2 = 1.0;
  annulus_sample_test ( center, r1, r2 );
%
%  Terminate.
%
  fprintf ( 1, '\n' );
  fprintf ( 1, 'annulus_monte_carlo_test():\n' );
  fprintf ( 1, '  Normal end of execution.\n' );
  fprintf ( 1, '\n' );
  timestamp ( );

  rmpath ( '../annulus_monte_carlo' )

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

