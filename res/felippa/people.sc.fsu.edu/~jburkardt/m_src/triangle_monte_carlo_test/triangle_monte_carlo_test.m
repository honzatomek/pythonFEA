function triangle_monte_carlo_test ( )

%*****************************************************************************80
%
%% triangle_monte_carlo_test() tests triangle_monte_carlo().
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    06 April 2019
%
%  Author:
%
%    John Burkardt
%
  addpath ( '../triangle_monte_carlo' );

  timestamp ( );
  fprintf ( 1, '\n' );
  fprintf ( 1, 'triangle_monte_carlo_test()\n' );
  fprintf ( 1, '  MATLAB/Octave version %s\n', version ( ) );
  fprintf ( 1, '  Test triangle_monte_carlo().\n' );
%
%  Try each sampler on the unit triangle, integrating X^2, X*Y, Y^2.
%
  triangle_monte_carlo_test01 ( );
  triangle_monte_carlo_test02 ( );
  triangle_monte_carlo_test03 ( );
  triangle_monte_carlo_test04 ( );
%
%  Try each sampler on a general triangle, integrating a selection of functions.
%
  triangle_monte_carlo_test05 ( );
  triangle_monte_carlo_test06 ( );
  triangle_monte_carlo_test07 ( );
  triangle_monte_carlo_test08 ( );
%
%  Terminate.
%
  fprintf ( 1, '\n' );
  fprintf ( 1, 'triangle_monte_carlo_test():\n' );
  fprintf ( 1, '  Normal end of execution.\n' );
  fprintf ( 1, '\n' );
  timestamp ( );

  rmpath ( '../triangle_monte_carlo' );

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

