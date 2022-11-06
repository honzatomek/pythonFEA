function simplex_gm_rule_test05 ( )

%*****************************************************************************80
%
%% simplex_gm_rule_test05() tests gm_unit_rule_set().
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
  test_num = 4;

  m_test = [ 2, 3, 5, 10 ];

  fprintf ( 1, '\n' );
  fprintf ( 1, 'SIMPLEX_GM_RULE_TEST05\n' );
  fprintf ( 1, '  GM_UNIT_RULE_SET determines the weights and abscissas\n' );
  fprintf ( 1, '  of a Grundmann-Moeller quadrature rule for\n' );
  fprintf ( 1, '  the M dimensional unit simplex,\n' );
  fprintf ( 1, '  using a rule of index RULE,\n' );
  fprintf ( 1, '  which will have degree of exactness 2*RULE+1.\n' );
  fprintf ( 1, '\n' );
  fprintf ( 1, '  In this test, we compute various rules, and simply\n' );
  fprintf ( 1, '  report the number of points, and the sum of weights.\n' );

  fprintf ( 1, '\n' );
  fprintf ( 1, '   M            RULE    N  WEIGHT SUM\n' );

  for test = 1 : test_num

    m = m_test(test);

    fprintf ( 1, '\n' );

    for rule = 0 : 5

      n = gm_rule_size ( rule, m );

      [ w, x ] = gm_unit_rule_set ( rule, m, n );

      w_sum = sum ( w(1:n) );

      fprintf ( 1, '  %8d  %8d  %8d  %24.16f\n', ...
        m, rule, n, w_sum );

    end

  end

  return
end
