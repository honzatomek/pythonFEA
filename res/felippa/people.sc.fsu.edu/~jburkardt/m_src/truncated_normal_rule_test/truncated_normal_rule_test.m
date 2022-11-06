function truncated_normal_rule_test ( )

%*****************************************************************************80
%
%% truncated_normal_rule_test() tests truncated_normal_rule().
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    11 April 2019
%
%  Author:
%
%    John Burkardt
%
  addpath ( '../truncated_normal_rule' )

  timestamp ( );
  fprintf ( 1, '\n' );
  fprintf ( 1, 'truncated_normal_rule_test():\n' );
  fprintf ( 1, '  MATLAB/Octave version %s\n', version ( ) );
  fprintf ( 1, '  Test truncated_normal_rule().\n' );

  truncated_normal_rule ( 0, 5, 1.0, 2.0, 'option0' );
  truncated_normal_rule ( 1, 9, 2.0, 0.5, 0.0, 'option1' );
  truncated_normal_rule ( 2, 9, 2.0, 0.5, 3.0, 'option2' );
  truncated_normal_rule ( 3, 5, 100.0, 25.0, 50.0, 150.0, 'option3' );
%
%  Terminate.
%
  fprintf ( 1, '\n' );
  fprintf ( 1, 'truncated_normal_rule_test():\n' );
  fprintf ( 1, '  Normal end of execution.\n' );
  fprintf ( 1, '\n' );
  timestamp ( );

  rmpath ( '../truncated_normal_rule' )

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

