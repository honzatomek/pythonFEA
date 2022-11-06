function triangle01_monte_carlo_test ( )

%*****************************************************************************80
%
%% triangle01_monte_carlo_test() tests triangle01_monte_carlo().
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    05 April 2019
%
%  Author:
%
%    John Burkardt
%
  addpath ( '../triangle01_monte_carlo' );

  timestamp ( );
  fprintf ( 1, '\n' );
  fprintf ( 1, 'triangle01_monte_carlo_test():\n' );
  fprintf ( 1, '  MATLAB/Octave version %s\n', version ( ) );
  fprintf ( 1, '  Test triangle01_monte_carlo().\n' );

  triangle01_monte_carlo_test01 ( );
%
%  Terminate.
%
  fprintf ( 1, '\n' );
  fprintf ( 1, 'triangle01_monte_carlo_test():\n' );
  fprintf ( 1, '  Normal end of execution.\n' );
  fprintf ( 1, '\n' );
  timestamp ( );

  rmpath ( '../triangle01_monte_carlo' );

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

