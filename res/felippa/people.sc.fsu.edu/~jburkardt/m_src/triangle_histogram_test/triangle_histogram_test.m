function triangle_histogram_test ( )

%*****************************************************************************80
%
%% triangle_histogram_test() tests triangle_histogram().
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    18 February 2019
%
%  Author:
%
%    John Burkardt
%
  addpath ( '../triangle_histogram' )

  timestamp ( );
  fprintf ( 1, '\n' );
  fprintf ( 1, 'triangle_histogram_test():\n' );
  fprintf ( 1, '  MATLAB/Octave version %s\n', version ( ) );
  fprintf ( 1, '  Test triangle_histogram().\n' );

  triangle_histogram ( 'b10000.txt', 4 );
  triangle_histogram ( 'g10000.txt', 4 );
%
%  Terminate.
%
  fprintf ( 1, '\n' );
  fprintf ( 1, 'triangle_histogram_test():\n' );
  fprintf ( 1, '  Normal end of execution.\n' );
  fprintf ( 1, '\n' );
  timestamp ( );

  rmpath ( '../triangle_histogram' )

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

