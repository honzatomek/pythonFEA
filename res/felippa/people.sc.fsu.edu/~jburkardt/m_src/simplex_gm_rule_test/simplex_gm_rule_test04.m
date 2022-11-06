function simplex_gm_rule_test04 ( )

%*****************************************************************************80
%
%% simplex_gm_rule_test04() tests gm_unit_rule_set().
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
  fprintf ( 1, 'SIMPLEX_GM_RULE_TEST04\n' );
  fprintf ( 1, '  GM_UNIT_RULE_SET determines the weights and abscissas\n' );
  fprintf ( 1, '  of a Grundmann-Moeller quadrature rule for\n' );
  fprintf ( 1, '  the M dimensional unit simplex,\n' );
  fprintf ( 1, '  using a rule of index RULE,\n' );
  fprintf ( 1, '  which will have degree of exactness 2*RULE+1.\n' );

  m = 3;
  rule = 2;

  fprintf ( 1, '\n' );
  fprintf ( 1, '  Here we use M = %d\n', m );
  fprintf ( 1, '  RULE = %d\n', rule );
  fprintf ( 1, '  DEGREE = %d\n', 2 * rule + 1 );

  n = gm_rule_size ( rule, m );

  [ w, x ] = gm_unit_rule_set ( rule, m, n );

  fprintf ( 1, '\n' );
  fprintf ( 1, '     POINT        W             X             Y             Z\n' );
  fprintf ( 1, '\n' );

  for point = 1 : n
    fprintf ( 1, '  %8d  %12f', point, w(point) );
    for dim = 1 : m
      fprintf ( 1, '  %12f', x(dim,point) );
    end
    fprintf ( 1, '\n' );
  end

  return
end
