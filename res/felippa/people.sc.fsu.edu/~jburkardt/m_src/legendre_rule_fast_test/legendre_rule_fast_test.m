function legendre_rule_fast_test ( )

%*****************************************************************************80
%
%% legendre_rule_fast_test() tests legendre_rule_fast().
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    14 February 2019
%
%  Author:
%
%    John Burkardt
%
  addpath ( '../legendre_rule_fast' )

  timestamp ( );
  fprintf ( 1, '\n' );
  fprintf ( 1, 'legendre_rule_fast_test():\n' );
  fprintf ( 1, '  MATLAB/Octave version %s\n', version ( ) );
  fprintf ( 1, '  Test legendre_rule_fast().\n' );

  legendre_rule_fast ( 15, 0.0, 2.0 );
%
%  Terminate.
%
  fprintf ( 1, '\n' );
  fprintf ( 1, 'legendre_rule_fast_test():\n' );
  fprintf ( 1, '  Normal end of execution.\n' );
  fprintf ( 1, '\n' );
  timestamp ( );

  rmpath ( '../legendre_rule_fast' )

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

