function annulus_rule_test ( )

%*****************************************************************************80
%
%% annulus_rule_test() tests annulus_rule().
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    06 July 2018
%
%  Author:
%
%    John Burkardt
%
  addpath ( '../annulus_rule' )

  timestamp ( );
  fprintf ( 1, '\n' );
  fprintf ( 1, 'annulus_rule_test():\n' );
  fprintf ( 1, '  MATLAB/Octave version %s\n', version ( ) );
  fprintf ( 1, '  Test annulus_rule().\n' );

  annulus_area_test ( );
  annulus_rule_compute_test ( );
  annulus_rule_plot_test ( );

  center = [ 0.0, 0.0 ];
  r1 = 0.0;
  r2 = 1.0;
  annulus_rule_monomial_test ( center, r1, r2 );

  center = [ 0.0, 0.0 ];
  r1 = 0.5;
  r2 = 1.0;
  annulus_rule_monomial_test ( center, r1, r2 );

  center = [ 1.0, 0.0 ];
  r1 = 0.0;
  r2 = 1.0;
  annulus_rule_monomial_test ( center, r1, r2 );
%
%  Terminate.
%
  fprintf ( 1, '\n' );
  fprintf ( 1, 'annulus_rule_test():\n' );
  fprintf ( 1, '  Normal end of execution.\n' );
  fprintf ( 1, '\n' );
  timestamp ( );

  rmpath ( '../annulus_rule' )

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

