function square_minimal_rule_test ( )

%*****************************************************************************80
%
%% square_minimal_rule_test() tests square_minimal_rule().
%
%  Licensing:
%
%    This code is distributed under the GNU GPL license.
%
%  Modified:
%
%    23 March 2019
%
%  Author:
%
%    John Burkardt.
%
  addpath ( '../square_minimal_rule' );

  timestamp ( );
  fprintf ( 1, '\n' );
  fprintf ( 1, 'square_minimal_rule_test():\n' );
  fprintf ( 1, '  MATLAB/Octave version %s\n', version ( ) );
  fprintf ( 1, '  Test square_minimal_rule().\n' );

  degree = 8;
  square_minimal_rule_print_test ( degree );

  square_minimal_rule_order_test ( );

  square_minimal_rule_error_max_test ( );
%
%  Terminate.
%
  fprintf ( 1, '\n' );
  fprintf ( 1, 'square_minimal_rule_test():\n' );
  fprintf ( 1, '  Normal end of execution.\n' );
  fprintf ( 1, '\n' );
  timestamp ( );

  rmpath ( '../square_minimal_rule' );

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

