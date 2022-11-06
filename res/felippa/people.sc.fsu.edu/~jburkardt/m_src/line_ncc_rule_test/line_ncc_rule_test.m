function line_ncc_rule_test ( )

%*****************************************************************************80
%
%% line_ncc_rule_test() tests line_ncc_rule().
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license. 
%
%  Modified:
%
%    15 February 2019
%
%  Author:
%
%    John Burkardt
%
  addpath ( '../line_ncc_rule' );

  timestamp ( );
  fprintf ( 1, '\n' );
  fprintf ( 1, 'line_ncc_rule_test():\n' );
  fprintf ( 1, '  MATLAB/Octave version %s\n', version ( ) );
  fprintf ( 1, '  line_ncc_rule() computes closed Newton Cotes\n' );
  fprintf ( 1, '  quadrature rules for a line segment.\n' );

  line_ncc_rule_test01 ( );
  line_ncc_rule_test02 ( );
%
%  Terminate.
%
  fprintf ( 1, '\n' );
  fprintf ( 1, 'line_ncc_rule_test():\n' );
  fprintf ( 1, '  Normal end of execution.\n' );
  fprintf ( 1, '\n' );
  timestamp ( );

  rmpath ( '../line_ncc_rule' );

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

