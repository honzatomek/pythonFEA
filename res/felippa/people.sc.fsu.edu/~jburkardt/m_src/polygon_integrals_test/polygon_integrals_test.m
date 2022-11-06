function polygon_integrals_test ( )

%*****************************************************************************80
%
%% polygon_integrals_test() tests polygon_integrals().
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
  addpath ( '../polygon_integrals' );

  timestamp ( );
  fprintf ( '\n' );
  fprintf ( 'polygon_integrals_test():\n' );
  fprintf ( 1, '  MATLAB/Octave version %s\n', version ( ) );
  fprintf ( '  Test polygon_integrals().\n' );

  polygon_integrals_test01 ( );
%
%  Terminate.
%
  fprintf ( '\n' );
  fprintf ( 'polygon_integrals_test():\n' );
  fprintf ( '  Normal end of execution.\n' );
  fprintf ( '\n' );
  timestamp ( );

  rmpath ( '../polygon_integrals' );

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

