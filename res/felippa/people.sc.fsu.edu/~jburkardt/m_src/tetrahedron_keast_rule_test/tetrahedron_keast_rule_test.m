function tetrahedron_keast_rule_test ( )

%*****************************************************************************80
%
%% tetrahedron_keast_rule_test() tests tetrahedron_keast_rule().
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    03 April 2019
%
%  Author:
%
%    John Burkardt
%
  addpath ( '../tetrahedron_keast_rule' );

  timestamp ( );
  fprintf ( 1, '\n' );
  fprintf ( 1, 'tetrahedron_keast_rule_test():\n' );
  fprintf ( 1, '  MATLAB/Octave version %s\n', version ( ) );
  fprintf ( 1, '  Test tetrahedron_keast_rule().\n' );

  tetrahedron_keast_rule_test01 ( );
  tetrahedron_keast_rule_test02 ( );
  tetrahedron_keast_rule_test025 ( );
  tetrahedron_keast_rule_test03 ( );
  tetrahedron_keast_rule_test04 ( );
  tetrahedron_keast_rule_test05 ( );
  tetrahedron_keast_rule_test06 ( );
%
%  Terminate.
%
  fprintf ( 1, '\n' );
  fprintf ( 1, 'tetrahedron_keast_rule_test():\n' );
  fprintf ( 1, '  Normal end of execution.\n' );
  fprintf ( 1, '\n' );
  timestamp ( );

  rmpath ( '../tetrahedron_keast_rule' );

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

