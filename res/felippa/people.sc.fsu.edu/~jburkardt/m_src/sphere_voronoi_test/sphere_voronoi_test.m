function sphere_voronoi_test ( )

%*****************************************************************************80
%
%% sphere_voronoi_test() tests sphere_voronoi().
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license. 
%
%  Modified:
%
%    27 March 2019
%
%  Author:
%
%    John Burkardt
%
  addpath ( '../sphere_voronoi' );

  timestamp ( );
  fprintf ( 1, '\n' );
  fprintf ( 1, 'sphere_voronoi_test():\n' );
  fprintf ( 1, '  MATLAB/Octave version %s\n', version ( ) );
  fprintf ( 1, '  Test sphere_voronoi().\n' );

  sphere_voronoi_test01 ( );
  sphere_voronoi_test02 ( );
  sphere_voronoi_test03 ( );
  sphere_voronoi_test04 ( );
%
%  Terminate.
%
  fprintf ( 1, '\n' );
  fprintf ( 1, 'sphere_voronoi_test():\n' );
  fprintf ( 1, '  Normal end of execution.\n' );
  fprintf ( 1, '\n' );
  timestamp ( );

  rmpath ( '../sphere_voronoi' );

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

