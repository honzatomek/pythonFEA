function pyramid_exactness_test ( )

%*****************************************************************************80
%
%% pyramid_exactness_test() tests pyramid_exactness().
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    05 March 2019
%
%  Author:
%
%    John Burkardt
%
  addpath ( '../pyramid_exactness' )

  timestamp ( );
  fprintf ( 1, '\n' );
  fprintf ( 1, 'pyramid_exactness_test():\n' );
  fprintf ( 1, '  MATLAB/Octave version %s\n', version ( ) );
  fprintf ( 1, '  Test pyramid_exactness().\n' );

  pyramid_exactness ( 'pyramid_l3x3_j3', 6 );
%
%  Terminate.
%
  fprintf ( 1, '\n' );
  fprintf ( 1, 'pyramid_exactness_test():\n' );
  fprintf ( 1, '  Normal end of execution.\n' );
  fprintf ( 1, '\n' );
  timestamp ( );

  rmpath ( '../pyramid_exactness' )

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

