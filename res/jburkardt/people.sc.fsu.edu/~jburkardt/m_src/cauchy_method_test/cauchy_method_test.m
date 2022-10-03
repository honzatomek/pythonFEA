function cauchy_method_test ( )

%*****************************************************************************80
%
%% cauchy_method_test tests cauchy_method.
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
  addpath ( '../cauchy_method' );

  timestamp ( );
  fprintf ( 1, '\n' );
  fprintf ( 1, 'cauchy_method_test:\n' );
  fprintf ( 1, '  MATLAB/Octave version %s\n', version ( ) );
  fprintf ( 1, '  Test the Cauchy method.\n' );
  fprintf ( 1, '  We use a constant stepsize.\n' );

  tspan = [ 0.0, 1.0 ];
  y0 = 0.0;
  n = 40;

  for theta = [ 0.25, 0.5, 0.75 ]
    stiff_cauchy_fixed_test ( tspan, y0, n, theta );
  end

  for theta = [ 0.25, 0.5, 0.75 ]
    stiff_cauchy_fsolve_test ( tspan, y0, n, theta );
  end
%
%  Terminate.
%
  fprintf ( 1, '\n' );
  fprintf ( 1, 'cauchy_method_test:\n' );
  fprintf ( 1, '  Normal end of execution.\n' );
  fprintf ( 1, '\n' );
  timestamp ( );

  rmpath ( '../cauchy_method' )

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

