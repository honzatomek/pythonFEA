function triangle_lyness_rule_test01 ( )

%*****************************************************************************80
%
%% triangle_lyness_rule_test01() tests lyness_rule_num(), lyness_degree(), lyness_order_num().
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
  fprintf ( 1, 'TRIANGLE_LYNESS_RULE_TEST01\n' );
  fprintf ( 1, '  LYNESS_RULE_NUM returns the number of rules;\n' );
  fprintf ( 1, '  LYNESS_DEGREE returns the degree of a rule;\n' );
  fprintf ( 1, '  LYNESS_ORDER_NUM returns the order of a rule.\n' );

  rule_num = lyness_rule_num ( );

  fprintf ( 1, '\n' );
  fprintf ( 1, '  Number of available rules = %d\n', rule_num );
  fprintf ( 1, '\n' );
  fprintf ( 1, '      Rule     Order  Precision\n' );
  fprintf ( 1, '\n' );

  for rule = 0 : rule_num
    order = lyness_order ( rule );
    precision = lyness_precision ( rule );
    fprintf ( 1, '  %8d  %8d  %8d\n', rule, order, precision );
  end

  return
end
