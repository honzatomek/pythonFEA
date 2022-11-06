function annulus_rule_plot_test ( )

%*****************************************************************************80
%
%% annulus_rule_plot_test() tests annulus_rule_plot().
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
  fprintf ( 1, '\n' );
  fprintf ( 1, 'ANNULUS_RULE_PLOT_TEST:\n' );
  fprintf ( 1, '  ANNULUS_RULE_PLOT plots annulus quadrature rule points.\n' );

  center = [ 0.0, 0.0 ];
  r1 = 0.5;
  r2 = 1.0;
  nr = 3;
  nt = 12;
  filename = 'annulus_rule_plot_test.png';
  label = '3 R x 12 theta annulus rule:';

  [ w, x, y ] = annulus_rule_compute ( center, r1, r2, nr, nt );

  annulus_rule_plot ( center, r1, r2, nr, nt, w, x, y, filename, label );
%
%  Terminate.
%
  fprintf ( 1, '\n' );
  fprintf ( 1, 'ANNULUS_RULE_PLOT_TEST:\n' );
  fprintf ( 1, '  Normal end of execution.\n' );

  return
end
 
