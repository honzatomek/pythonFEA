function tetrahedron_arbq_rule_test ( )

%*****************************************************************************80
%
%% tetrahedron_arbq_rule_test() tests tetrahedron_arbq_rule().
%
%  Licensing:
%
%    This code is distributed under the GNU GPL license.
%
%  Modified:
%
%    03 April 2019
%
%  Author:
%
%    John Burkardt.
%
%  Reference:
%
%    Hong Xiao, Zydrunas Gimbutas,
%    A numerical algorithm for the construction of efficient quadrature
%    rules in two and higher dimensions,
%    Computers and Mathematics with Applications,
%    Volume 59, 2010, pages 663-676.
%
  addpath ( '../tetrahedron_arbq_rule' );

  timestamp ( );
  fprintf ( 1, '\n' );
  fprintf ( 1, 'tetrahedron_arbq_rule_test():\n' );
  fprintf ( 1, '  MATLAB/Octave version %s\n', version ( ) );
  fprintf ( 1, '  Test tetrahedron_arbq_rule().\n' );

  degree = 8;
  n = tetrahedron_arbq_size ( degree );
  header = 'tetrahedron08';

  tetrahedron_arbq_rule_test01 ( degree, n );

  tetrahedron_arbq_rule_test02 ( degree, n, header );

  tetrahedron_arbq_rule_test03 ( degree, n, header );

  tetrahedron_arbq_rule_test04 ( degree, n );
%
%  Terminate.
%
  fprintf ( 1, '\n' );
  fprintf ( 1, 'tetrahedron_arbq_rule_test():\n' );
  fprintf ( 1, '  Normal end of execution.\n' );
  fprintf ( 1, '\n' );
  timestamp ( );

  rmpath ( '../tetrahedron_arbq_rule' );

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

