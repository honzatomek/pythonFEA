function ccn_rule_test ( )

%*****************************************************************************80
%
%% ccn_rule_test() tests ccn_rule().
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    11 December 2018
%
%  Author:
%
%    John Burkardt
%
  addpath ( '../ccn_rule' )

  timestamp ( );
  fprintf ( 1, '\n' );
  fprintf ( 1, 'CCN_RULE_TEST\n' );
  fprintf ( 1, '  MATLAB/Octave version %s.\n', version ( ) );
  fprintf ( 1, '  Test CCN_RULE.\n' );

  n = 9;
  a = -1.0;
  b = +1.0;
  filename = 'ccn_o9';

  ccn_rule ( n, a, b, filename );
%
%  Terminate.
%
  fprintf ( 1, '\n' );
  fprintf ( 1, 'CCN_RULE_TEST\n' );
  fprintf ( 1, '  Normal end of execution.\n' );
  fprintf ( 1, '\n' );
  timestamp ( );

  rmpath ( '../ccn_rule' )

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

