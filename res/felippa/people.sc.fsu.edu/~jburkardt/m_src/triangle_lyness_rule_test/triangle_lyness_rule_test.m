function triangle_lyness_rule_test ( )

%*****************************************************************************80
%
%% triangle_lyness_rule_test() tests triangle_lyness_rule().
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    06 April 2019
%
%  Author:
%
%    John Burkardt
%
  addpath ( '../triangle_lyness_rule' );

  timestamp ( );
  fprintf ( 1, '\n' );
  fprintf ( 1, 'triangle_lyness_rule_test():\n' );
  fprintf ( 1, '  MATLAB/Octave version %s\n', version ( ) );
  fprintf ( 1, '  Test triangle_lyness_rule()\n' );

  triangle_lyness_rule_test01 ( );
  triangle_lyness_rule_test02 ( );
  triangle_lyness_rule_test03 ( );
  triangle_lyness_rule_test04 ( );
  triangle_lyness_rule_test05 ( );
  triangle_lyness_rule_test06 ( );
%
%  Terminate.
%
  fprintf ( 1, '\n' );
  fprintf ( 1, 'triangle_lyness_rule_test():\n' );
  fprintf ( 1, '  Normal end of execution.\n' );
  fprintf ( 1, '\n' );
  timestamp ( );

  rmpath ( '../triangle_lyness_rule' );

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

