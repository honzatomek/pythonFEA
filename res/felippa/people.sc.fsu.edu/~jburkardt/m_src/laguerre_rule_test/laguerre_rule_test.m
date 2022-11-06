function laguerre_rule_test ( )

%*****************************************************************************80
%
%% laguerre_rule_test() tests laguerre_rule().
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    11 February 2019
%
%  Author:
%
%    John Burkardt
%
  addpath ( '../laguerre_rule' )

  timestamp ( );
  fprintf ( 1, '\n' );
  fprintf ( 1, 'laguerre_rule_test():\n' );
  fprintf ( 1, '  MATLAB/Octave version %s\n', version ( ) );
  fprintf ( 1, '  Test laguerre_rule().\n' );

  laguerre_rule ( 4, 0.0, 1.0, 'lag_o4' );
%
%  Terminate.
%
  fprintf ( 1, '\n' );
  fprintf ( 1, 'laguerre_rule_test():\n' );
  fprintf ( 1, '  Normal end of execution.\n' );
  fprintf ( 1, '\n' );
  timestamp ( );

  rmpath ( '../laguerre_rule' )

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

