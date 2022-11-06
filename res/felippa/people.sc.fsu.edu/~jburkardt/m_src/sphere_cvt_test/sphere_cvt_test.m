function sphere_cvt_test ( )

%*****************************************************************************80
%
%% sphere_cvt_test() tests sphere_cvt().
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license. 
%
%  Modified:
%
%    24 March 2019
%
%  Author:
%
%    John Burkardt
%
  addpath ( '../sphere_cvt' );

  timestamp ( );
  fprintf ( 1, '\n' );
  fprintf ( 1, 'sphere_cvt_test():\n' );
  fprintf ( 1, '  MATLAB/Octave version %s\n', version ( ) );
  fprintf ( 1, '  Test sphere_cvt().\n' );

  sphere_cvt_test01 ( );
%
%  Terminate.
%
  fprintf ( 1, '\n' );
  fprintf ( 1, 'sphere_cvt_test():\n' );
  fprintf ( 1, '  Normal end of execution.\n' );
  fprintf ( 1, '\n' );
  timestamp ( );

  rmpath ( '../sphere_cvt' );

  return
end
function sphere_cvt_test01 ( )

%*****************************************************************************80
%
%% sphere_cvt_test01() tests the computation of a CVT on the unit sphere.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license. 
%
%  Modified:
%
%    03 May 2010
%
%  Author:
%
%    John Burkardt
%
  fprintf ( 1, '\n' );
  fprintf ( 1, 'SPHERE_CVT_TEST01\n' );
  fprintf ( 1, '  Demonstrate the iterative computation of a CVT\n' );
  fprintf ( 1, '  (Centroidal Voronoi Tessellation) on the unit sphere.\n' );
%
%  Choose a random set of points on the unit sphere.
%
  n = 100;
  xyz = uniform_on_sphere01_map ( 3, n );
  r8mat_transpose_print ( 3, n, xyz, '  Initial points:' );

  figure ( 1 )
  sphere_voronoi_plot ( n, xyz )

  for i = 1 : 50

    centroid = sphere_cvt_step ( n, xyz );

    xyz(1:3,1:n) = centroid(1:3,1:n);

  end

  r8mat_transpose_print ( 3, n, xyz, '  Final points:' );
%
%  Plot the final data.
%
  figure ( 2 )
  sphere_voronoi_plot ( n, xyz );

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

