function polygon_monte_carlo_test ( )

%*****************************************************************************80
%
%% polygon_monte_carlo_test() tests polygon_monte_carlo().
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    26 February 2019
%
%  Author:
%
%    John Burkardt
%
  addpath ( '../polygon_monte_carlo' );
  addpath ( '../polygon_triangulate' );

  timestamp ( );
  fprintf ( 1, '\n' );
  fprintf ( 1, 'polygon_monte_carlo_test():\n' );
  fprintf ( 1, '  MATLAB/Octave version %s\n', version ( ) );
  fprintf ( 1, '  Test polygon_monte_carlo().\n' );

  nv1 = 4;

  v1 = [ ...
    -1.0, -1.0; ...
     1.0, -1.0; ...
     1.0,  1.0; ...
    -1.0,  1.0 ]';

  polygon_monte_carlo_test01 ( nv1, v1 );
%
%  Terminate.
%
  fprintf ( 1, '\n' );
  fprintf ( 1, 'polygon_monte_carlo_test():\n' );
  fprintf ( 1, '  Normal end of execution.\n' );
  fprintf ( 1, '\n' );
  timestamp ( );

  rmpath ( '../polygon_monte_carlo' );
  rmpath ( '../polygon_triangulate' );

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

