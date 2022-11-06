function triangle_lyness_rule_test04 ( )

%*****************************************************************************80
%
%% triangle_lyness_rule_test04() prints a rule generated by lyness_rule().
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    30 September 2010
%
%  Author:
%
%    John Burkardt
%
  fprintf ( 1, '\n' );
  fprintf ( 1, 'TRIANGLE_LYNESS_RULE_TEST04\n' );
  fprintf ( 1, '  LYNESS_RULE returns the points and weights\n' );
  fprintf ( 1, '  of a Lyness rule for the triangle.\n' );
  fprintf ( 1, '\n' );
  fprintf ( 1, '  In this test, we simply print a rule.\n' );

  rule = 18;
  order = lyness_order ( rule );
  precision = lyness_precision ( rule );

  fprintf ( 1, '\n' );
  fprintf ( 1, '  Rule =      %d\n', rule );
  fprintf ( 1, '  Order =     %d\n', order );
  fprintf ( 1, '  Precision = %d\n', precision );

  [ w, x ] = lyness_rule ( rule, order );

  fprintf ( 1, '\n' );
  fprintf ( 1, '         I      W               X               Y\n' );
  fprintf ( 1, '\n' );

  for j = 1 : order
    fprintf ( 1, '  %8d  %24.16f  %24.16f  %24.16f\n', j, w(j), x(1:2,j) );
  end

  return
end
