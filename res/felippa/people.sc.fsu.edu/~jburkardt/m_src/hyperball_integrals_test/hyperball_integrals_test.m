function hyperball_integrals_test ( )

%*****************************************************************************80
%
%% hyperball_integrals_test() tests hyperball_integrals().
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    31 January 2019
%
%  Author:
%
%    John Burkardt
%
  addpath ( '../hyperball_integrals' );

  timestamp ( );
  fprintf ( 1, '\n' );
  fprintf ( 1, 'hyperball_integrals_test():\n' );
  fprintf ( 1, '  MATLAB/Octave version %s\n', version ( ) );
  fprintf ( 1, '  Test hyperball_integrals().\n' );

  hyperball_integrals_test01 ( );
  hyperball_integrals_test02 ( );
%
%  Terminate.
%
  fprintf ( 1, '\n' );
  fprintf ( 1, 'hyperball_integrals_test():\n' );
  fprintf ( 1, '  Normal end of execution.\n' );
  fprintf ( 1, '\n' );
  timestamp ( );

  rmpath ( '../hyperball_integrals' );

  return;
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

