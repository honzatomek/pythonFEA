function theta_method_test ( )

%*****************************************************************************80
%
%% theta_method_test() tests theta_method().
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    01 March 2021
%
%  Author:
%
%    John Burkardt
%
  addpath ( '../theta_method' );

  timestamp ( );
  fprintf ( 1, '\n' );
  fprintf ( 1, 'theta_method_test():\n' );
  fprintf ( 1, '  MATLAB/Octave version %s\n', version ( ) );
  fprintf ( 1, '  Test theta method().\n' );
  fprintf ( 1, '  We use a constant stepsize.\n' );

  tspan = [ 0.0, 1.0 ];
  y0 = 0.0;
  n = 27;
  for theta = [ 0.0, 0.5, 1.0 ]
    stiff_theta_method_test ( tspan, y0, n, theta );
  end
%
%  Terminate.
%
  fprintf ( 1, '\n' );
  fprintf ( 1, 'theta_method_test():\n' );
  fprintf ( 1, '  Normal end of execution.\n' );
  fprintf ( 1, '\n' );
  timestamp ( );

  rmpath ( '../theta_method' )

  return
end
function timestamp ( )

%*****************************************************************************80
%
%% timestamp prints the current YMDHMS date as a timestamp.
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

