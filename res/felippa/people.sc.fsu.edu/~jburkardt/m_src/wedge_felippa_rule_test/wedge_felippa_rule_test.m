function wedge_felippa_rule_test ( )

%*****************************************************************************80
%
%% wedge_felippa_rule_test() tests wedge_felippa_rule().
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license. 
%
%  Modified:
%
%    07 February 2019
%
%  Author:
%
%    John Burkardt
%
  addpath ( '../wedge_felippa_rule' );

  timestamp ( );
  fprintf ( 1, '\n' );
  fprintf ( 1, 'wedge_felippa_rule_test():\n' );
  fprintf ( 1, '  MATLAB/Octave version %s.\n', version ( ) );
  fprintf ( 1, '  Test wedge_felippa_rule()\n' );

  max_degree = 4;

  wedge_felippa_rule_test01 ( max_degree );
  wedge_felippa_rule_test02 ( max_degree );
  wedge_felippa_rule_test03 ( max_degree );
%
%  Terminate.
%
  fprintf ( 1, '\n' );
  fprintf ( 1, 'wedge_felippa_rule_test():\n' );
  fprintf ( 1, '  Normal end of execution.\n' );
  fprintf ( 1, '\n' );
  timestamp ( );

  rmpath ( '../wedge_felippa_rule' );

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

