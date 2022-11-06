function cube_felippa_rule_test ( )

%*****************************************************************************80
%
%% cube_felippa_rule_test() tests cube_felippa_rule().
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license. 
%
%  Modified:
%
%    22 December 2018
%
%  Author:
%
%    John Burkardt
%
  addpath ( '../cube_felippa_rule' );

  timestamp ( );
  fprintf ( 1, '\n' );
  fprintf ( 1, 'CUBE_FELIPPA_RULE_TEST()\n' );
  fprintf ( 1, '  MATLAB/Octave version %s.\n', version ( ) );
  fprintf ( 1, '  Test CUBE_FELIPPA_RULE().\n' );

  degree_max = 4;
  cube_monomial_test ( degree_max );

  degree_max = 6;
  cube_quad_test ( degree_max );
%
%  Terminate.
%
  fprintf ( 1, '\n' );
  fprintf ( 1, 'CUBE_FELIPPA_RULE_TEST()\n' );
  fprintf ( 1, '  Normal end of execution.\n' );
  fprintf ( 1, '\n' );
  timestamp ( );

  rmpath ( '../cube_felippa_rule' );

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

