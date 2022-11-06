function sphere_lebedev_rule_test01 ( )

%*****************************************************************************80
%
%% sphere_lebedev_rule_test01() tests available_table(), order_table(), precision_table().
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license. 
%
%  Modified:
%
%   13 September 2010
%
%  Author:
%
%    John Burkardt
%
  rule_max = 65;

  fprintf ( 1, '\n' );
  fprintf ( 1, 'SPHERE_LEBEDEV_RULE_TEST01\n' );
  fprintf ( 1, '  List Lebedev rule properties.\n' );
  fprintf ( 1, '\n' );
  fprintf ( 1, '  Rule Avail Order  Prec\n' );
  fprintf ( 1, '\n' );
  for rule = 1 : rule_max
    available = available_table ( rule );
    order = order_table ( rule );
    precision = precision_table ( rule );
    fprintf ( 1, '  %4d  %4d  %4d  %4d\n', rule, available, order, precision );
  end

  return
end
