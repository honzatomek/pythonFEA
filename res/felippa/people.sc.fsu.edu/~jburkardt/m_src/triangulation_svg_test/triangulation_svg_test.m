function triangulation_svg_test ( )

%*****************************************************************************80
%
%% triangulation_svg_test() tests triangulation_svg().
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
  addpath ( '../triangulation_svg' )

  timestamp ( );
  fprintf ( 1, '\n' );
  fprintf ( 1, 'triangulation_svg_test():\n' );
  fprintf ( 1, '  MATLAB/Octave version %s\n', version ( ) );
  fprintf ( 1, '  Test triangulation_svg().\n' );

  triangulation_svg ( 'lake' );
%
%  Terminate.
%
  fprintf ( 1, '\n' );
  fprintf ( 1, 'triangulation_svg_test():\n' );
  fprintf ( 1, '  Normal end of execution.\n' );
  fprintf ( 1, '\n' );
  timestamp ( );

  rmpath ( '../triangulation_svg' )

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

