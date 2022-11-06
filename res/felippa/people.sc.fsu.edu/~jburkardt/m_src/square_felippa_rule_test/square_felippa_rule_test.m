function square_felippa_rule_test ( )

%*****************************************************************************80
%
%% square_felippa_rule_test() tests square_felippa_rule().
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license. 
%
%  Modified:
%
%    23 March 2019
%
%  Author:
%
%    John Burkardt
%
  addpath ( '../square_felippa_rule' );

  timestamp ( );
  fprintf ( 1, '\n' );
  fprintf ( 1, 'square_felippa_rule_test():\n' );
  fprintf ( 1, '  MATLAB/Octave version %s\n', version ( ) );
  fprintf ( 1, '  Test square_felippa_rule().\n' );

  degree_max = 4;
  square_monomial_test ( degree_max );

  degree_max = 5;
  square_quad_test ( degree_max );
%
%  Terminate.
%
  fprintf ( 1, '\n' );
  fprintf ( 1, 'square_felippa_rule_test():\n' );
  fprintf ( 1, '  Normal end of execution.\n' );
  fprintf ( 1, '\n' );
  timestamp ( );

  rmpath ( '../square_felippa_rule' );

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

