function simplex_gm_rule_test06 ( )

%*****************************************************************************80
%
%% simplex_gm_rule_test06() tests gm_unit_rule_set().
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    09 July 2007
%
%  Author:
%
%    John Burkardt
%
  fprintf ( 1, '\n' );
  fprintf ( 1, 'SIMPLEX_GM_RULE_TEST06\n' );
  fprintf ( 1, '  GM_UNIT_RULE_SET determines the weights and abscissas\n' );
  fprintf ( 1, '  of a Grundmann-Moeller quadrature rule for\n' );
  fprintf ( 1, '  the M dimensional unit simplex,\n' );
  fprintf ( 1, '  using a rule of index RULE,\n' );
  fprintf ( 1, '  which will have degree of exactness 2*RULE+1.\n' );
  fprintf ( 1, '\n' );
  fprintf ( 1, '  In this test, we write a rule to a file.\n' );
  fprintf ( 1, '\n' );

  m = 3;
  rule = 2;

  fprintf ( 1, '\n' );
  fprintf ( 1, '  Here we use M = %d\n', m );
  fprintf ( 1, '  RULE = %d\n', rule );
  fprintf ( 1, '  DEGREE = %d\n', 2 * rule + 1 );

  n = gm_rule_size ( rule, m );

  [ w, x ] = gm_unit_rule_set ( rule, m, n );

  w_file = sprintf ( 'gm%d_%dd_w.txt', rule, m );

  w_unit = fopen ( w_file, 'wt' );

  for point = 1 : n
    fprintf ( w_unit, '%f20.16\n', w(point) );
  end

  fclose ( w_unit );

  x_file = sprintf ( 'gm%d_%dd_x.txt', rule, m );

  x_unit = fopen ( x_file, 'wt' );

  for point = 1 : n
    for dim = 1 : m
      fprintf ( x_unit, '%20.16f', x(dim,point) );
    end
    fprintf ( x_unit, '\n' );
  end

  fclose ( x_unit );

  fprintf ( 1, '\n' );
  fprintf ( 1, '  Wrote rule %d to "%s" and "%s".\n', rule, w_file, x_file );

  return
end
