function simplex_coordinates_test ( )

%*****************************************************************************80
%
%% simplex_coordinates_test() tests simplex_coordinates().
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    16 March 2019
%
%  Author:
%
%    John Burkardt
%
  addpath ( '../simplex_coordinates' );

  timestamp ( );
  fprintf ( 1, '\n' );
  fprintf ( 1, 'simplex_coordinates_test():\n' );
  fprintf ( 1, '  MATLAB/Octave version %s\n', version ( ) );
  fprintf ( 1, '  Test simplex_coordinates().\n' );

  n = 3;
  simplex_coordinates1_test ( n );
  simplex_coordinates2_test ( n );

  n = 4;
  simplex_coordinates1_test ( n );
  simplex_coordinates2_test ( n );
%
%  Terminate.
%
  fprintf ( 1, '\n' );
  fprintf ( 1, 'simplex_coordinates_test():\n' );
  fprintf ( 1, '  Normal end of execution.\n' );
  fprintf ( 1, '\n' );
  timestamp ( );

  rmpath ( '../simplex_coordinates' );

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

