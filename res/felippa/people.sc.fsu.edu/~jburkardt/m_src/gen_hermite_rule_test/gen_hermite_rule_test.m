function gen_hermite_rule_test ( )

%*****************************************************************************80
%
%% gen_hermite_rule_test() tests gen_hermite_rule().
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    24 January 2019
%
%  Author:
%
%    John Burkardt
%
  addpath ( '../gen_hermite_rule' )

  timestamp ( );
  fprintf ( 1, '\n' );
  fprintf ( 1, 'gen_hermite_rule_test():\n' );
  fprintf ( 1, '  MATLAB/Octave version %s\n', version ( ) );
  fprintf ( 1, '  Test gen_hermite_rule().\n' );

  gen_hermite_rule ( 4, 1.0, 0.0, 1.0, 'gen_herm_o4_a1.0' );
%
%  Terminate.
%
  fprintf ( 1, '\n' );
  fprintf ( 1, 'gen_hermite_rule_test():\n' );
  fprintf ( 1, '  Normal end of execution.\n' );
  fprintf ( 1, '\n' );
  timestamp ( );

  rmpath ( '../gen_hermite_rule' )

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

