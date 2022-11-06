function tetrahedron_exactness_test ( )

%*****************************************************************************80
%
%% tetrahedron_exactness_test() tests tetrahedron_exactness().
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    03 April 2019
%
%  Author:
%
%    John Burkardt
%
  addpath ( '../tetrahedron_exactness' )

  timestamp ( );
  fprintf ( 1, '\n' );
  fprintf ( 1, 'tetrahedron_exactness_test():\n' );
  fprintf ( 1, '  MATLAB/Octave version %s\n', version ( ) );
  fprintf ( 1, '  Test tetrahedron_exactness().\n' );

  tetrahedron_exactness ( 'keast7', 7 );
%
%  Terminate.
%
  fprintf ( 1, '\n' );
  fprintf ( 1, 'tetrahedron_exactness_test():\n' );
  fprintf ( 1, '  Normal end of execution.\n' );
  fprintf ( 1, '\n' );
  timestamp ( );

  rmpath ( '../tetrahedron_exactness' )

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

