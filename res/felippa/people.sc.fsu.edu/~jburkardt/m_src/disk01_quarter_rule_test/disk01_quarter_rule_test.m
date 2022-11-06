function disk01_quarter_rule_test ( )

%*****************************************************************************80
%
%% disk01_quarter_rule_test() tests disk01_quarter_rule().
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    09 January 2019
%
%  Author:
%
%    John Burkardt
%
  addpath ( '../disk01_quarter_rule' );

  timestamp ( );
  fprintf ( 1, '\n' );
  fprintf ( 1, 'DISK01_QUARTER_RULE_TEST():\n' );
  fprintf ( 1, '  MATLAB/Octave version %s\n', version ( ) );
  fprintf ( 1, '  Test DISK01_QUARTER_RULE().\n' );

  n = 3;
  d = 2;

  disk01_quarter_rule ( n, d );
%
%  Terminate.
%
  fprintf ( 1, '\n' );
  fprintf ( 1, 'DISK01_QUARTER_RULE_TEST():\n' );
  fprintf ( 1, '  Normal end of execution.\n' );
  fprintf ( 1, '\n' );
  timestamp ( );

  rmpath ( '../disk01_quarter_rule' );

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

