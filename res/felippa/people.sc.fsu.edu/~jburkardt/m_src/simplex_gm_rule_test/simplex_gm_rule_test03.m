function simplex_gm_rule_test03 ( )

%*****************************************************************************80
%
%% simplex_gm_rule_test03() tests gm_rule_size().
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
  fprintf ( 1, 'SIMPLEX_GM_RULE_TEST03\n' );
  fprintf ( 1, '  GM_RULE_SIZE returns N, the number of points\n' );
  fprintf ( 1, '  associated with a Grundmann-Moeller quadrature rule\n' );
  fprintf ( 1, '  for the unit simplex of dimension M\n' );
  fprintf ( 1, '  with rule index RULE\n' );
  fprintf ( 1, '  and degree of exactness DEGREE = 2*RULE+1.\n' );

  fprintf ( 1, '\n' );
  fprintf ( 1, '         M      RULE    DEGREE N\n' );

  for test = 1 : test_num

    m = m_test(test);

    fprintf ( 1, '\n' );

    for rule = 0 : 5

      n = gm_rule_size ( rule, m );
      degree = 2 * rule + 1;

      fprintf ( 1, '  %8d  %8d  %8d  %8d\n', m, rule, degree, n );

    end

  end

  return
end
