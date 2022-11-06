function gnuplot_test ( )

%*****************************************************************************80
%
%% gnuplot_test() tests gnuplot().
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    06 December 2020
%
%  Author:
%
%    John Burkardt
%
  addpath ( '../gnuplot' );

  timestamp ( );
  fprintf ( 1, '\n' );
  fprintf ( 1, 'gnuplot_test():\n' );
  fprintf ( 1, '  MATLAB/Octave version %s\n', version ( ) );
  fprintf ( 1, '  Test gnuplot().\n' );

  damped_sine ( );
  string_simulation ( );
%
%  Terminate.
%
  fprintf ( 1, '\n' );
  fprintf ( 1, 'gnuplot_test():\n' );
  fprintf ( 1, '  Normal end of execution.\n' );
  fprintf ( 1, '\n' );
  timestamp ( );

  rmpath ( '../gnuplot' );

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

