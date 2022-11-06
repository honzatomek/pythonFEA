function triangle_fekete_rule_test ( )

%*****************************************************************************80
%
%% triangle_fekete_rule_test() tests triangle_fekete_rule().
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    05 April 2019
%
%  Author:
%
%    John Burkardt
%
  addpath ( '../triangle_fekete_rule' );

  timestamp ( );
  fprintf ( 1, '\n' );
  fprintf ( 1, 'triangle_fekete_rule_test():\n' );
  fprintf ( 1, '  MATLAB/Octave version %s\n', version ( ) );
  fprintf ( 1, '  Test triangle_fekete_rule().\n' );

  triangle_fekete_rule_test01 ( );
  triangle_fekete_rule_test02 ( );
  triangle_fekete_rule_test025 ( );
  triangle_fekete_rule_test03 ( );
  triangle_fekete_rule_test04 ( );
  triangle_fekete_rule_test05 ( );
  triangle_fekete_rule_test06 ( );
%
%  Terminate.
%
  fprintf ( 1, '\n' );
  fprintf ( 1, 'triangle_fekete_rule_test():\n' );
  fprintf ( 1, '  Normal end of execution.\n' );
  fprintf ( 1, '\n' );
  timestamp ( );

  rmpath ( '../triangle_fekete_rule' );

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

