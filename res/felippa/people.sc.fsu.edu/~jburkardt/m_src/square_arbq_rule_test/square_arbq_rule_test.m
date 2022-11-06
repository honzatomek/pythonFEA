function square_arbq_rule_test ( )

%*****************************************************************************80
%
%% square_arbq_rule_test() tests square_arbq_rule().
%
%  Licensing:
%
%    This code is distributed under the GNU GPL license.
%
%  Modified:
%
%    21 March 2019
%
%  Author:
%
%    Original FORTRAN77 version by Hong Xiao, Zydrunas Gimbutas.
%    MATLAB version by John Burkardt.
%
%  Reference:
%
%    Hong Xiao, Zydrunas Gimbutas,
%    A numerical algorithm for the construction of efficient quadrature
%    rules in two and higher dimensions,
%    Computers and Mathematics with Applications,
%    Volume 59, 2010, pages 663-676.
%
  addpath ( '../square_arbq_rule' );

  timestamp ( );
  fprintf ( 1, '\n' );
  fprintf ( 1, 'square_arbq_rule_test():\n' );
  fprintf ( 1, '  MATLAB/Octave version %s\n', version ( ) );
  fprintf ( 1, '  Test square_arbq_rule().\n' );

  degree = 8;
  n = square_arbq_size ( degree );
  header = 'square08';

  square_arbq_rule_test01 ( degree, n );

  square_arbq_rule_test02 ( degree, n, header );

  square_arbq_rule_test03 ( degree, n, header );

  square_arbq_rule_test04 ( degree, n );
%
%  Terminate.
%
  fprintf ( 1, '\n' );
  fprintf ( 1, 'square_arbq_rule_test():\n' );
  fprintf ( 1, '  Normal end of execution.\n' );
  fprintf ( 1, '\n' );
  timestamp ( );

  rmpath ( '../square_arbq_rule' );

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

