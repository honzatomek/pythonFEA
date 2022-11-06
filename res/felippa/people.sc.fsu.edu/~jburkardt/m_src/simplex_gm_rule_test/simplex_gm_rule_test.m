function simplex_gm_rule_test ( )

%*****************************************************************************80
%
%% simplex_gm_rule_test() tests simplex_gm_rule().
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    16 March 2019
%
%  Author:
%
%    John Burkardt
%
  addpath ( '../simplex_gm_rule' );

  timestamp ( );
  fprintf ( 1, '\n' );
  fprintf ( 1, 'simplex_gm_rule_test():\n' );
  fprintf ( 1, '  MATLAB/Octave version %s\n', version ( ) );
  fprintf ( 1, '  Test simplex_gm_rule().\n' );

  simplex_gm_rule_test01 ( );
  simplex_gm_rule_test02 ( );
  simplex_gm_rule_test03 ( );
  simplex_gm_rule_test04 ( );
  simplex_gm_rule_test05 ( );
  simplex_gm_rule_test06 ( );
  simplex_gm_rule_test07 ( );
  simplex_gm_rule_test08 ( );
  simplex_gm_rule_test09 ( );
  simplex_gm_rule_test10 ( );
%
%  Terminate.
%
  fprintf ( 1, '\n' );
  fprintf ( 1, 'simplex_gm_rule_test():\n' );
  fprintf ( 1, '  Normal end of execution.\n' );
  fprintf ( 1, '\n' );
  timestamp ( );

  rmpath ( '../simplex_gm_rule' );

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

